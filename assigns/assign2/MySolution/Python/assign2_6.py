# Assignment 2-6 -- cli50@bu.edu -- for CS320
#############

import sys
sys.setrecursionlimit(16000)
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if i1 < n1 then (
#   if i2 < n2 then
#     let c1 = string_get_at(cs1)(i1)
#     and c2 = string_get_at(cs2)(i2) in
#     if c1 <= c2 then (work(c1); foreach(i1+1)(i2+0)(work))
#     else (work(c2); foreach(i1+0)(i2+1)(work))
#   else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;

def string_merge(cs1, cs2): 
  def string_make_fwork(fwork):
    res = []
    def work(char):
      res.append(char)
    
    fwork(work)

    return "".join(res)

  def string_get_at(str, i):
    return str[i]

  def string_length(str):
    return len(str)

  n1 = string_length(cs1)
  n2 = string_length(cs2)
  def foreach(i1, i2, work):
    if i1 < n1:
      if i2 < n2:
        c1 = string_get_at(cs1, i1)
        c2 = string_get_at(cs2, i2)
        if c1 <= c2:
          work(c1)
          foreach(i1+1, i2+0, work)
        else:
          work(c2)
          foreach(i1+0, i2+1, work)
      else:
        int1_foreach(n1 - i1, lambda i: work(string_get_at(cs1, i1+i)))
    else:
      int1_foreach(n2 - i2, lambda i: work(string_get_at(cs2, i2+i)))
  
  res = string_make_fwork(lambda work: foreach(0, 0, work))

  return res


print(string_merge("135", "2468"))
print(string_merge("abcde", "1234"))
print(string_merge("12345", "abcd"))