rvrs s = third ++ " " ++ second ++ " " ++ first
    where first = take (length "curry") s
          second = take 2 $ drop (1 + length first) s
          third = take 7 $ drop (2 + length first + length second) s
