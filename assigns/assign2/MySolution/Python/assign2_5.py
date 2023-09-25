# Assignment 2-5 -- cli50@bu.edu -- for CS320
#############

import sys
sys.setrecursionlimit(16000)
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# (** transforms the work done by fwork into a list. **)
# let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
#   let res = ref([]) in
#     let work(x0) = (res := (x0 :: !res))
#     in(*let*)(fwork(work); list_reverse(!res) )
# ;;

# def fnlist_make_fwork(fwork):
def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)
    return fnlist_reverse(res)

print(fnlist_make_fwork(print))