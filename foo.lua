local m = {}

function m.on_tick(n)
   print("In on_tick; tick == " .. n)
   print("My position is (" .. me.getx() .. ", " .. me.gety() .. ")")
end

return m
