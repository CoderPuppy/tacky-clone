local pl = require 'pl.import_into' ()

local function xtend(...)
	local t = {}
	for i = 1, select('#', ...) do
		t = pl.tablex.merge(t, select(i, ...), true)
	end
	return t
end

local function parse(str)
	local str = str:match '^%s*(.*)$'

	local rest = str:match '^%((.*)$'
	if rest then
		local vals = {}
		local str = rest
		local done = false
		while not done do
			repeat
				local rest = str:match '^%s*%)(.*)$'
				if rest then
					str = rest
					done = true
					break
				end

				if #str == 0 then
					return nil, str, 'unmatched parens'
				end

				local val, rest, err = parse(str)
				if val then
					vals[#vals + 1] = val
					str = rest
					break
				else
					return nil, rest, err
				end
			until true
		end
		return vals, str
	end

	local val, rest = str:match '^(%d+)(.*)$'
	if not val then
		val, rest = str:match '^(%d+%.%d*)(.*)$'
	end
	if val then
		return tonumber(val), rest
	end

	local quote, rest = str:match '^([\'"])(.*)$'
	if quote then
		local val = ''
		local done = false
		str = rest
		while not done do
			repeat
				local esc, rest = str:match '^(\\[abfnrtv\\"\'0])(.*)$'
				if not esc then
					esc, rest = str:match '^(\\x%x%x)(.*)$'
				end
				if not esc then
					esc, rest = str:match '^(\\%d%d%d)(.*)$'
				end
				if not esc then
					esc, rest = str:match '^(\\u{%x+})(.*)$'
				end
				if esc then
					val = val .. load('return "' .. esc .. '"')()
					str = rest
					break
				end

				local rest = str:match '^\\\n(.*)$'
				if rest then
					val = val .. '\n'
					str = rest
					break
				end

				local rest = str:match '^\\z%s*(.*)$'
				if rest then
					str = rest
					break
				end

				local part, rest = str:match('^([^' .. quote .. '\\]+)(.*)$')
				if part then
					val = val .. part
					str = rest
					break
				end

				local rest = str:match('^' .. quote .. '(.*)$')
				if rest then
					str = rest
					done = true
					break
				end

				error(('bad: %q'):format(str))
			until true
		end
		return { 'quote', val }, str
	end
	
	local n, contents, rest = str:match '^%[(=*)%[(.-)%]%1%](.*)$'
	if n then
		return { 'quote', contents }, rest
	end

	local rest = str:match '^`(.*)$'
	if rest then
		local val, rest, err = parse(rest)
		if val then
			return { 'quote', val }, rest
		else
			return nil, rest, err
		end
	end

	local rest = str:match '^,(.*)$'
	if rest then
		local val, rest, err = parse(rest)
		if val then
			return { 'unquote', val }, rest
		else
			return nil, rest, err
		end
	end

	local val, rest = str:match '^([^%s)]+)(.*)$'
	if val then
		return val, rest
	end

	return nil, str, 'nothing matched'
end

local compile, compile_quote, emit

function compile_quote(sexp, ctx)
	if type(sexp) == 'string' then
		return { 'string', sexp }
	elseif type(sexp) == 'number' then
		return { 'number', sexp }
	elseif sexp[1] == 'unquote' then
		return compile(sexp[2], ctx)
	else
		local ir = { 'table' }
		for i = 1, #sexp do
			ir[#ir + 1] = {{'table', {compile_quote(sexp[i], ctx)}}}
		end
		return { 'call', { '.', { 'call', { 'var', 'require' }, { { 'string', 'pl.array2d' } } }, { 'string', 'flatten' } }, {ir} }
	end
end

function compile(sexp, ctx)
	if type(sexp) == 'string' then
		return { 'var', sexp }
	elseif type(sexp) == 'number' then
		return { 'num', sexp }
	elseif sexp == nil then
		return { 'var', 'nil' }
	elseif sexp[1] == 'do' then
		local ir = { 'block' }
		for i = 2, #sexp do
			ir[#ir + 1] = compile(sexp[i], ctx)
		end
		return ir
	elseif sexp[1] == 'local' then
		local ir = { 'local', {}, {} }
		for i = 1, #sexp[2] do
			ir[2][#ir[2] + 1] = compile(sexp[2][i], ctx)
		end
		for i = 3, #sexp do
			ir[3][#ir[3] + 1] = compile(sexp[i], ctx)
		end
		return ir
	elseif sexp[1] == 'set' then
		local ir = { 'set', {}, {} }
		for i = 1, #sexp[2] do
			ir[2][#ir[2] + 1] = compile(sexp[2][i], ctx)
		end
		for i = 3, #sexp do
			ir[3][#ir[3] + 1] = compile(sexp[i], ctx)
		end
		return ir
	elseif sexp[1] == 'lambda' then
		local ir = { 'lambda', sexp[2], { 'block' } }
		for i = 3, #sexp do
			ir[3][#ir[3] + 1] = compile(sexp[i], ctx)
		end
		return ir
	elseif sexp[1] == 'unquote' then
		local macros = {}
		local ir = compile({ 'do', table.unpack(sexp, 2) }, {
			macros = macros;
			env = setmetatable({
				macros = macros;
			}, { __index = _G });
		})
		local src = emit(ir, {
			type = 'statement';
			value = true;
			indent = 0;
		})
		print('-- unquote')
		print(src)
		print('-- done')
		local fn, err = load(src, 'unquote', nil, ctx.env)
		if not fn then
			error(err)
		end
		return compile(fn(), ctx)
	elseif sexp[1] == 'quote' then
		return compile_quote(sexp[2], ctx)
	elseif sexp[1] == '.' then
		return { '.', compile(sexp[2], ctx), compile(sexp[3], ctx) }
	elseif ctx.macros[sexp[1]] then
		return compile(ctx.macros[sexp[1]](table.unpack(sexp, 2)), ctx)
	elseif #sexp >= 1 then
		local ir =  { 'call', compile(sexp[1], ctx), {} }
		for i = 2, #sexp do
			ir[3][#ir[3] + 1] = compile(sexp[i], ctx)
		end
		return ir
	else
		error(('bad: %s in %s'):format(pl.pretty.write(sexp), pl.pretty.write(ctx)))
	end
end

function emit(ir, ctx)
	if ir[1] == 'call' then
		local out = ''
		if ctx.type == 'statement' and ctx.ret then
			out = out .. 'return '
		end
		out = out .. emit(ir[2], xtend(ctx, {
			type = 'expression';
			value = true;
		}))
		out = out .. '('
		for i = 1, #ir[3] do
			if i ~= 1 then
				out = out .. ', '
			end
			out = out .. emit(ir[3][i], xtend(ctx, {
				type = 'expression';
				value = true;
			}))
		end
		out = out .. ')'
		if ctx.type == 'statement' then
			out = out .. '\n'
		end
		return out
	elseif ir[1] == 'var' then
		if ctx.value then
			local out = ''
			if ctx.type == 'statement' then
				out = out .. 'return '
			end
			out = out .. ir[2]
			if ctx.type == 'statement' then
				out = out .. ';\n'
			end
			return out
		elseif ctx.type == 'statement' then
			return ('(function() return %s end)();\n'):format(ir[2])
		else
			error('wat')
		end
	elseif ir[1] == 'string' then
		if ctx.value then
			local out = ''
			if ctx.type == 'statement' then
				out = out .. 'return '
			end
			out = out .. ('%q'):format(ir[2])
			if ctx.type == 'statement' then
				out = out .. ';\n'
			end
			return out
		elseif ctx.type == 'statement' then
			return ''
		else
			error('wat')
		end
	elseif ir[1] == 'lambda' then
		if ctx.value then
			local out = ''
			out = out .. 'function('
			for i = 1, #ir[2] do
				if i ~= 1 then
					out = out .. ', '
				end
				out = out .. ir[2][i]
			end
			out = out .. ');\n'
			out = out .. emit(ir[3], xtend(ctx, {
				type = 'statement';
				value = true;
				indent = ctx.indent + 1;
			}))
			out = out .. 'end'
			return out
		elseif ctx.type == 'statement' then
			return ''
		else
			error('wat')
		end
	elseif ir[1] == 'set' then
		local left = ''
		for i = 1, #ir[2] do
			if i ~= 1 then
				left = left .. ', '
			end
			left = left .. emit(ir[2][i], xtend(ctx, {
				type = 'expression';
				value = true;
			}))
		end

		local right = ''
		for i = 1, #ir[3] do
			if i ~= 1 then
				right = right .. ', '
			end
			right = right .. emit(ir[3][i], xtend(ctx, {
				type = 'expression';
				value = true;
			}))
		end

		if ctx.type == 'statement' then
			if ctx.value then
				return ('return (function(...) %s = ...; return ... end)(%s);\n'):format(left, right)
			else
				return ('%s = %s;\n'):format(left, right)
			end
		elseif ctx.type == 'expression' then
			return ('(function(...) %s = ...; return ... end)(%s)'):format(left, right)
		else
			error('wat')
		end
	elseif ir[1] == '.' then
		local out = ''
		if ctx.type == 'statement' then
			if not ctx.value then
				out = out .. '(function() '
			end
			out = out .. 'return '
		end
		out = out .. emit(ir[2], xtend(ctx, {
			type = 'expression';
			value = true;
		}))
		out = out .. '['
		out = out .. emit(ir[3], xtend(ctx, {
			type = 'expression';
			value = true;
		}))
		out = out .. ']'
		if ctx.type == 'statement' then
			if not ctx.value then
				out = out .. ' end)()'
			end
			out = out .. ';\n'
		end
		return out
	elseif ir[1] == 'block' then
		local out = ''
		if ctx.type == 'expression' then
			out = out .. '(function(...)\n'
		end
		for i = 2, #ir - 1 do
			out = out .. emit(ir[i], xtend(ctx, {
				type = 'statement';
				value = false;
			}))
		end
		if #ir > 1 then
			out = out .. emit(ir[#ir], xtend(ctx, {
				type = 'statement';
			}))
		end
		if ctx.type == 'expression' then
			if ctx.vararg then
				out = out .. 'end)(...)'
			else
				out = out .. 'end)()'
			end
		end
		return out
	elseif ir[1] == 'table' then
		local out = ''
		if ctx.type == 'statement' then
			if not ctx.value then
				out = out .. '(function() '
			end
			out = out .. 'return '
		end
		out = out .. '{\n'
		for i = 2, #ir do
			local entry = ir[i]
			if #entry == 1 then
				out = out .. ('%s;\n'):format(
					emit(entry[1], xtend(ctx, {
						type = 'expression';
						value = true;
					}))
				)
			elseif #entry == 2 then
				out = out .. ('[%s] = %s;\n'):format(
					emit(entry[1], xtend(ctx, {
						type = 'expression';
						value = true;
					})),
					emit(entry[2], xtend(ctx, {
						type = 'expression';
						value = true;
					}))
				)
			else
				error('wat')
			end
		end
		out = out .. '}'
		if ctx.type == 'statement' then
			if not ctx.value then
				out = out .. ' end)()'
			end
			out = out .. ';\n'
		end
		return out
	elseif ir[1] == 'local' then
		local left = ''
		for i = 1, #ir[2] do
			if i ~= 1 then
				left = left .. ', '
			end
			left = left .. emit(ir[2][i], xtend(ctx, {
				type = 'expression';
				value = true;
			}))
		end

		local right = ''
		for i = 1, #ir[3] do
			if i ~= 1 then
				right = right .. ', '
			end
			right = right .. emit(ir[3][i], xtend(ctx, {
				type = 'expression';
				value = true;
			}))
		end

		if ctx.type == 'statement' then
			if ctx.value then
				return ('local %s = %s;\nreturn %s'):format(left, right, left)
			else
				return ('local %s = %s;\n'):format(left, right)
			end
		elseif ctx.type == 'value' then
			return right
		else
			error('wat')
		end
	else
		error(('bad: %s in %s'):format(pl.pretty.write(ir), pl.pretty.write(ctx)))
	end
end

local code = [========[
(do
	(unquote
		(set ((. macros "unquote_nil")) (lambda (...) (do
			`(,"unquote" (do ,... nil))
		)))
		nil
	)
	(unquote
		(print "test")
		`(print "hello?")
	)
	(unquote_nil
		(print "test")
		`(print "hello?")
	)
)
]========]

local sexp, rest, err = parse(code)
if not sexp then
	error(('%s at %q'):format(err, rest))
end
print('sexp', pl.pretty.write(sexp))

local ir
do
	local macros = {}
	ir = compile(sexp, {
		macros = macros;
		env = setmetatable({
			macros = macros;
		}, { __index = _G });
	})
end
print('ir', pl.pretty.write(ir))

local src = emit(ir, {
	type = 'statement';
	value = true;
})
print('-- final')
print(src)
print('-- done')

local fn, err = load(src, 'final', nil, { pl = pl })
if not fn then
	error(err)
end
fn()
