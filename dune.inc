
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
    