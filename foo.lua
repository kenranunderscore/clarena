local m = {}

function m.on_tick(tick)
   print("In on_tick; tick == " .. tick)
   print("My x is " .. me.x())
   local res = me.move(6.4)
   -- print("move.distance == " .. res.distance)
   return { res, me.turn_head(0.1), me.turn_arms(0.02) }
end

-- function m.on_round_started()
--    print("in round started")
--    return { me.move(-1.1) }
-- end

return m
