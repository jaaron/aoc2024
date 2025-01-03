
(rule
    (target day1.test.output)
    (deps examples/day1.txt)
    (action (with-stdout-to %{target} (run bin/day1.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day1.test.output examples/day1.output)))
    
(rule
    (target day10.test.output)
    (deps examples/day10.txt)
    (action (with-stdout-to %{target} (run bin/day10.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day10.test.output examples/day10.output)))
    
(rule
    (target day11.test.output)
    (deps examples/day11.txt)
    (action (with-stdout-to %{target} (run bin/day11.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day11.test.output examples/day11.output)))
    
(rule
    (target day12.test.output)
    (deps examples/day12.txt)
    (action (with-stdout-to %{target} (run bin/day12.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day12.test.output examples/day12.output)))
    
(rule
    (target day13.test.output)
    (deps examples/day13.txt)
    (action (with-stdout-to %{target} (run bin/day13.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day13.test.output examples/day13.output)))
    
(rule
    (target day14.test.output)
    (deps examples/day14.txt)
    (action (with-stdout-to %{target} (run bin/day14.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day14.test.output examples/day14.output)))
    
(rule
    (target day16.test.output)
    (deps examples/day16.txt)
    (action (with-stdout-to %{target} (run bin/day16.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day16.test.output examples/day16.output)))
    
(rule
    (target day19.test.output)
    (deps examples/day19.txt)
    (action (with-stdout-to %{target} (run bin/day19.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day19.test.output examples/day19.output)))
    
(rule
    (target day2.test.output)
    (deps examples/day2.txt)
    (action (with-stdout-to %{target} (run bin/day2.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day2.test.output examples/day2.output)))
    
(rule
    (target day21.test.output)
    (deps examples/day21.txt)
    (action (with-stdout-to %{target} (run bin/day21.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day21.test.output examples/day21.output)))
    
(rule
    (target day23.test.output)
    (deps examples/day23.txt)
    (action (with-stdout-to %{target} (run bin/day23.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day23.test.output examples/day23.output)))
    
(rule
    (target day3.test.output)
    (deps examples/day3.txt)
    (action (with-stdout-to %{target} (run bin/day3.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day3.test.output examples/day3.output)))
    
(rule
    (target day4.test.output)
    (deps examples/day4.txt)
    (action (with-stdout-to %{target} (run bin/day4.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day4.test.output examples/day4.output)))
    
(rule
    (target day5.test.output)
    (deps examples/day5.txt)
    (action (with-stdout-to %{target} (run bin/day5.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day5.test.output examples/day5.output)))
    
(rule
    (target day6.test.output)
    (deps examples/day6.txt)
    (action (with-stdout-to %{target} (run bin/day6.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day6.test.output examples/day6.output)))
    
(rule
    (target day7.test.output)
    (deps examples/day7.txt)
    (action (with-stdout-to %{target} (run bin/day7.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day7.test.output examples/day7.output)))
    
(rule
    (target day8.test.output)
    (deps examples/day8.txt)
    (action (with-stdout-to %{target} (run bin/day8.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day8.test.output examples/day8.output)))
    
(rule
    (target day9.test.output)
    (deps examples/day9.txt)
    (action (with-stdout-to %{target} (run bin/day9.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day9.test.output examples/day9.output)))
    