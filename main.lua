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

local compile, compile_quote

function compile_quote(sexp, ctx)
	if type(sexp) == 'string' then
		return ('%q'):format(sexp)
	elseif type(sexp) == 'number' then
		return tostring(sexp)
	elseif sexp[1] == 'unquote' then
		return compile({ 'do', table.unpack(sexp, 2) }, xtend(ctx, {
			type = 'expression';
		}))
	else
		local out = ''
		out = out .. '{'
		for i = 1, #sexp do
			if i ~= 1 then
				out = out .. ', '
			end
			out = out .. compile_quote(sexp[i], ctx)
		end
		out = out .. '}'
		return out
	end
end

function compile(sexp, ctx)
	if type(sexp) == 'string' and ctx.type == 'expression' then
		return sexp
	elseif type(sexp) == 'string' and ctx.type == 'statement' and ctx.ret then
		return ('return %s\n'):format(sexp)
	elseif type(sexp) == 'number' then
		return tostring(sexp)
	elseif sexp == nil and ctx.type == 'expression' then
		return 'nil'
	elseif sexp == nil and ctx.type == 'statement' then
		return ''
	elseif sexp[1] == 'do' and ctx.type == 'statement' then
		local out = 'do\n'
		for i = 2, #sexp - 1 do
			out = out .. compile(sexp[i], xtend(ctx, {
				ret = false;
			}))
		end
		if #sexp > 1 then
			out = out .. compile(sexp[#sexp], ctx)
		end
		out = out .. 'end\n'
		return out
	elseif sexp[1] == 'do' and ctx.type == 'expression' then
		local out = '(function(...)\n'
		for i = 2, #sexp - 1 do
			out = out .. compile(sexp[i], xtend(ctx, {
				type = 'statement';
				ret = false;
			}))
		end
		if #sexp > 1 then
			out = out .. compile(sexp[#sexp], xtend(ctx, {
				type = 'statement';
				ret = true;
			}))
		end
		out = out .. 'end)(...)'
		return out
	elseif sexp[1] == 'define' and ctx.type == 'statement' then
		return ('local %s = %s\n'):format(sexp[2], compile(sexp[3], xtend(ctx, {
			type = 'expression';
		})))
	elseif sexp[1] == 'lambda' and ctx.type == 'expression' then
		local out = ''
		out = out .. 'function('
		for i = 1, #sexp[2] do
			if i ~= 1 then
				out = out .. ', '
			end
			out = out .. sexp[2][i]
		end
		out = out .. ')\n'
		for i = 3, #sexp - 1 do
			out = out .. compile(sexp[i], xtend(ctx, {
				type = 'statement';
				ret = false;
			}))
		end
		if #sexp > 2 then
			out = out .. compile(sexp[#sexp], xtend(ctx, {
				type = 'statement';
				ret = true;
			}))
		end
		out = out .. 'end'
		return out
	elseif sexp[1] == 'unquote' then
		local macros = {}
		local src = compile({ 'do', table.unpack(sexp, 2) }, {
			type = 'statement';
			ret = true;
			macros = macros;
			env = setmetatable({
				macros = macros;
			}, { __index = _G });
		})
		print('-- unquote')
		print(src)
		local fn, err = load(src, 'unquote', nil, ctx.env)
		if not fn then
			error(err)
		end
		return compile(fn(), ctx)
	elseif sexp[1] == 'quote' then
		local out = ''
		if ctx.type == 'statement' and ctx.ret then
			out = out .. 'return '
		end
		out = out .. compile_quote(sexp[2], ctx)
		if ctx.type == 'statement' then
			out = out .. '\n'
		end
		return out
	elseif sexp[1] == '.' and ctx.type == 'expression' then
		return ('%s[%s]'):format(
			compile(sexp[2], ctx),
			compile(sexp[3], ctx)
		)
	elseif sexp[1] == '.=' and ctx.type == 'statement' then
		local ctx_ = xtend(ctx, { type = 'expression'; })
		return ('%s[%s] = %s\n'):format(
			compile(sexp[2], ctx_),
			compile(sexp[3], ctx_),
			compile(sexp[4], ctx_)
		)
	elseif ctx.macros[sexp[1]] then
		return compile(ctx.macros[sexp[1]](table.unpack(sexp, 2)), ctx)
	elseif #sexp >= 1 then
		local out = ''
		if ctx.type == 'statement' and ctx.ret then
			out = out .. 'return '
		end
		out = out .. compile(sexp[1], xtend(ctx, {
			type = 'expression';
		})) .. '('
		for i = 2, #sexp do
			if i ~= 2 then
				out = out .. ', '
			end
			out = out .. compile(sexp[i], xtend(ctx, {
				type = 'expression';
			}))
		end
		out = out .. ')'
		if ctx.type == 'statement' then
			out = out .. '\n'
		end
		return out
	else
		error(('bad: %s in %s'):format(pl.pretty.write(sexp), pl.pretty.write(ctx)))
	end
end

local code = [========[
(do
	,(.= macros "unquote_nil" (lambda (...)
		(define pl ((require "pl.import_into")))
		(print ((. (. pl "pretty") "write") `(,...)))
		(print ((. (. pl "pretty") "write") `(,"unquote" (do ,... nil))))
		`(,"unquote" (do ,... nil))
	))
	(unquote_nil
		(print "a" "b" "c")
		,(print "shouldn't happen")
		(print "wat")
	)
)
]========]

local sexp, rest, err = parse(code)
if not sexp then
	error(('%s at %q'):format(err, rest))
end
print(pl.pretty.write(sexp))

local src
do
	local macros = {}
	src = compile(sexp, {
		type = 'statement';
		ret = true;
		macros = macros;
		env = setmetatable({
			macros = macros;
		}, { __index = _G });
	})
	print('-- final')
	print(src)
end

local fn, err = load(src, 'final', nil, { pl = pl })
if not fn then
	error(err)
end
fn()
