
# `apply` is only defined in Python2
# I couldn't figure out how to replicate func(*args) in Hy.
def apply(func, args):
    return func(*args)
