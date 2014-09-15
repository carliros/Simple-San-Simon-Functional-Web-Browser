module Utils.NumRomans where
import Data.Char

toRomanUpper :: Int -> String
toRomanUpper n = concat $ map convert $ zip ns [tns-1, tns-2 .. 0]
    where (ns, tns) = let ns = show n 
                      in (map digitToInt ns, length ns)
          convert (nchg, np) = case np of 
                                    0 -> ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! nchg
                                    1 -> ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] !! nchg
                                    2 -> ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"] !! nchg
                                    _ -> if nchg == 0 then "" else take (nchg * 10 ^ np `div` 1000) $ repeat 'M'

toRomanLower :: Int -> String
toRomanLower n = concat $ map convert $ zip ns [tns-1, tns-2 .. 0]
    where (ns, tns) = let ns = show n 
                      in (map digitToInt ns, length ns)
          convert (nchg, np) = case np of 
                                    0 -> ["", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"] !! nchg
                                    1 -> ["", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xm"] !! nchg
                                    2 -> ["", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm"] !! nchg
                                    _ -> if nchg == 0 then "" else take (nchg * 10 ^ np `div` 1000) $ repeat 'm'


