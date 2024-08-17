local m = {}

function m.on_tick(tick)
   print("In on_tick; tick == " .. tick)
   print("My position is (" .. me.x() .. ", " .. me.y() .. ")")
   local res = me.turn(tick * 2.3)
   print("Bound result, it's: " .. res.angle)
   return res
end

return m
