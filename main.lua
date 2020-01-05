local pl = require 'pl.import_into' ()

local old_type = type
local function type(v)
	local mt = getmetatable(v)
	if mt and mt.__type then
		return mt.__type(v)
	end
	return old_type(v)
end

local types = {}

local function xtend(...)
	local t = {}
	for i = 1, select('#', ...) do
		for k, v in pairs(select(i, ...)) do
			t[k] = v
		end
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

local function compile_scope(parent)
	local scope = {
		vars = {};
	}
	if parent then
		scope.parent = parent
		setmetatable(scope.vars, { __index = parent.vars; })
	end

	return scope
end

local function compile_ctx()
	local ctx = {
		scope = compile_scope();
	}

	return ctx
end

types.var = {}
local var_id = 0
local function genvar(ctx, var_)
	if type(var_) == types.var then
		return var_, ctx
	end

	local scope = compile_scope(ctx.scope)

	local var = {
		name = var_;
		id = var_id;
		scope = scope;
	}
	var_id = var_id + 1;
	setmetatable(var, {
		__type = types.var;
	})
	if var_ then
		scope.vars[var_] = var
	end

	return var, xtend(ctx, { scope = scope; })
end
local function getvar(ctx, var_)
	if type(var_) == types.var then
		local sc = ctx.scope
		while sc ~= var_.scope do
			sc = sc.parent
			if not sc then
				error('not in scope')
			end
		end
		return var_
	end

	if ctx.scope.vars[var_] then
		return ctx.scope.vars[var_]
	end
	
	return var_
end

local function compile(sexp, ctx)
	if type(sexp) == 'number' then
		return { 'number', sexp }, ctx
	elseif type(sexp) == 'string' then
		return { 'var', getvar(ctx, sexp) }, ctx
	elseif sexp[1] == 'do' then
		local octx = ctx
		local ir = { 'do' }
		for i = 2, #sexp do
			ir[#ir + 1], ctx = compile(sexp[i], ctx)
		end
		return { 'scope', ir }, xtend(ctx, {
			scope = octx.scope;
		})
	elseif sexp[1] == 'flat-do' then
		local ir = { 'do' }
		for i = 2, #sexp do
			ir[#ir + 1], ctx = compile(sexp[i], ctx)
		end
		return ir, ctx
	elseif sexp[1] == 'define' then
		local vars = {}
		if type(sexp[2]) == 'table' then
			for i = 1, #sexp[2] do
				vars[#var + 1], ctx = genvar(ctx, sexp[2][i])
			end
		else
			vars[1], ctx = genvar(ctx, sexp[2])
		end

		local vals = {}
		for i = 3, #sexp do
			vals[#vals + 1], ctx = compile(sexp[i], ctx)
		end

		return { 'local', vars, vals }, ctx
	elseif sexp[1] == 'set' then
		local vars = {}
		for i = 1, #sexp[2] do
			vars[#vars + 1], ctx = compile(sexp[2][i], ctx)
		end

		local vals = {}
		for i = 3, #sexp do
			vals[#vals + 1], ctx = compile(sexp[i], ctx)
		end

		return { 'set', vars, vals }, ctx
	elseif sexp[1] == 'quote' then

	elseif #sexp >= 1 then
		local fn; fn, ctx = compile(sexp[1], ctx)
		local args = {}
		for i = 2, #sexp do
			args[#args + 1], ctx = compile(sexp[i], ctx)
		end
		return { 'call', fn, args }, ctx
	else
		error(('bad: %s in %s'):format(pl.pretty.write(sexp), pl.pretty.write(ctx)))
	end
end

local code = [========[
(flat-do
	(define a 1)
	(print a)
)
]========]

local sexp, rest, err = parse(code)
if not sexp then
	error(('%s at %q'):format(err, rest))
end
print('sexp: ' .. pl.pretty.write(sexp))

local ir, ctx = compile(sexp, compile_ctx())
print('ir: ' .. pl.pretty.write(ir))
print('ctx: ' .. pl.pretty.write(ctx))
