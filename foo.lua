local m = {}

function m.on_tick(tick)
   print("In on_tick; tick == " .. tick)
   print("My position is (" .. me.x() .. ", " .. me.y() .. ")")
   local res = me.move(1.4)
   print("move.distance == " .. res.distance)
   return { res }
end

-- function m.on_round_started()
--    print("in round started")
--    return { me.move(-4.1) }
-- end

return m
