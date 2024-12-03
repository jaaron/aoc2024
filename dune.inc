
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
    