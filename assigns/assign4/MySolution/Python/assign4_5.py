# Assignment 4-5 -- cli50@bu.edu -- for CS320
#############

import sys
sys.setrecursionlimit(16000)
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
#   string_tabulate (string_length(cs)) (
#     fun i -> if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)

# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;

def string_fset_at(cs, i0, c0):
  n = len(cs)
  def string_tabulate(length, work):
    result = [work(i) for i in range(length)]
    return ''.join(result)

  def string_get_at(str, i):
    return str[i]

  def work(i):
    if i==i0:
      return c0
    else:
      return string_get_at(cs, i)

  return string_tabulate(n, lambda i: work(i) )

  

alphabet = "".join(chr(ord('a') + i) for i in range(26))

# let list_of_buddies (word: string): string list =
#   let n0 = string_length(word) 
#   in
#   list_make_fwork ( fun work ->
#      int1_foreach(n0) (
#       fun i0 -> let c0 = string_get_at(word)(i0)  in 
#       string_foreach(alphabet) (fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;; 


def list_of_buddies(word):
  n0 = len(word)
  
  def list_make_fwork(work):
    def int1_foreach(n0):
      for i0 in range(n0):
        c0 = word[i0]
        for c1 in alphabet:
          if c1 != c0:
            work(string_fset_at(word, i0, c1))
    int1_foreach(n0)
  
  result = []
  
  list_make_fwork(result.append)
  
  return result

# print(alphabet)
# print(string_fset_at("love", 1, 'j'))

# print(list_of_buddies("love"))