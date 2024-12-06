
(rule
    (target day1.test.output)
    (deps examples/day1.txt)
    (action (with-stdout-to %{target} (run bin/day1.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day1.test.output examples/day1.output)))
    
(rule
    (target day2.test.output)
    (deps examples/day2.txt)
    (action (with-stdout-to %{target} (run bin/day2.exe %{deps}))))
(rule
    (alias runtest)
    (action (diff day2.test.output examples/day2.output)))
    
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
    