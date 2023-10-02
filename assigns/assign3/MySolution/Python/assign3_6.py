# Assignment 3-6 -- cli50@bu.edu -- for CS320
#############

import sys
sys.setrecursionlimit(16000)
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# type 'a mylist =
#   | MyNil
#   | MyCons of 'a * 'a mylist
#   | MySnoc of 'a mylist * 'a
#   | MyReverse of 'a mylist
#   | MyAppend2 of 'a mylist * 'a mylist

# let rec
# mylist_foreach
# (xs: 'a mylist)
# (work: 'a -> unit): unit =
# match xs with
# | MyNil -> ()
# | MyCons(x1, xs) ->
#   (work(x1); mylist_foreach(xs)(work))
# | MySnoc(xs, x1) ->
#   (mylist_foreach(xs)(work); work(x1))
# | MyReverse(xs) -> mylist_rforeach(xs)(work)
# | MyAppend2(xs1, xs2) ->
#   (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

# and
# mylist_rforeach
# (xs: 'a mylist)
# (work: 'a -> unit): unit =
# match xs with
# | MyNil -> ()
# | MyCons(x1, xs) ->
#   (mylist_rforeach(xs)(work); work(x1))
# | MySnoc(xs, x1) ->
#   (work(x1); mylist_rforeach(xs)(work))
# | MyReverse(xs) -> mylist_foreach(xs)(work)
# | MyAppend2(xs1, xs2) ->
#   (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
# ;;

class MyList:
  def __init__(self):
    pass

class mylist_nil(MyList):
    pass

class mylist_cons(MyList):
  def __init__(self, head, tail):
    self.head = head
    self.tail = tail

class mylist_snoc(MyList):
  def __init__(self, init, last):
    self.init = init
    self.last = last

class mylist_reverse(MyList):
  def __init__(self, lst):
    self.lst = lst

class mylist_append2(MyList):
  def __init__(self, lst1, lst2):
    self.lst1 = lst1
    self.lst2 = lst2


def mylist_foreach(xs, work):
    if isinstance(xs, mylist_nil):
        pass  # Do nothing for MyNil
    elif isinstance(xs, mylist_cons):
        work(xs.head)
        mylist_foreach(xs.tail, work)
    elif isinstance(xs, mylist_snoc):
        mylist_foreach(xs.init, work)
        work(xs.last)
    elif isinstance(xs, mylist_reverse):
        mylist_rforeach(xs.lst, work)
    elif isinstance(xs, mylist_append2):
        mylist_foreach(xs.lst1, work)
        mylist_foreach(xs.lst2, work)

def mylist_rforeach(xs, work):
    if isinstance(xs, mylist_nil):
        pass  # Do nothing for MyNil
    elif isinstance(xs, mylist_cons):
        mylist_rforeach(xs.tail, work)
        work(xs.head)
    elif isinstance(xs, mylist_snoc):
        work(xs.last)
        mylist_rforeach(xs.init, work)
    elif isinstance(xs, mylist_reverse):
        mylist_foreach(xs.lst, work)
    elif isinstance(xs, mylist_append2):
        mylist_rforeach(xs.lst2, work)
        mylist_rforeach(xs.lst1, work)