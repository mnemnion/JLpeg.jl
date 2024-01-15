L = require "lpeglabel"

for k, v in pairs(L) do
    if k == 'type' then
        _G['ltype'] = v
    else
        _G[k] = v
    end
    --    print(k)
end

print "---"

p = P"1" * P"a"^1 + P"456"
match(p, "12")
print "---"
pcode(p)
print "---"

q = P"1" * P"2"^0 + P"456"
match(q, "12")
print "---"
pcode(q)
print "---"