# (*
# Q2-8: 20 points

# Recall the 'foreach' function and the 'get_at' function.
# For instance, list_foreach(xs)(work) applies 'work' to
# each element in the given list 'xs'; list_get_at(xs)(i)
# returns the element at position 'i' in 'xs' if 'i' is a
# valid index; otherwise the Subscript exception is raised.

# Please implement in *Python* a function 'foreach_to_get_at'
# that turns a 'foreach' function into a 'get_at' function.

# (*
# Following is the type for 'foreach_to_get_at' in ocaml:
# fun foreach_to_get_at
#   (foreach: ('xs, 'x0) foreach): ('xs -> int -> 'x0) = ...
# *)

#
# *)

  # lambda(xs, i):
  #   foreach(xs, lambda x: 
  #     curr_i = 0
  #     if(curr_i == i):
  #       return x
  #     else:
  #       curr_i += 1
  #     )

def foreach(lst, work):
  for val in lst: 
    work(val)

def foreach_to_get_at(foreach): # your implementation below
  def get_at_func(xs, i):
    curr_1 = 0
    def work(x):
      print(x)
      if curr_1 == i:
        return x
      else:
        curr_1+=1

    foreach(xs, lambda x:(
      work(x)
    ))

foreach_to_get_at(foreach([1,2,3], 2))