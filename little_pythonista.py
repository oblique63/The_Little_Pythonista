# Python translation of Scheme code originally presented in "The Little Schemer" (4th Ed)
# by: Daniel P. Friedman and Matthias Felleisen
# Translated by: Enrique Gavidia
#
# [This Is Meant For Educational Purposes Only]

#----{ Scheme jargon }-------------------------------------------------
def is_null(x):
    "Checks if 'x' is null"
    if type(x) == list:
        return len(x) == 0
    else:
        return False

def is_pair(x):
    "Checks if 'x' is a pair"
    if type(x) == list:
        return len(x) == 2
    else:
        return False
        
def car(l):
    "Returns first item in list l"
    return l[0]

def cdr(l):
    "Returns list l without first item"
    return l[1:]
    
def cons(a, b):
    "Constructs a pair (list) with 'a' and 'b'"
    return [a, b]
    
def is_equal(a, b):
    "Compares two objects for equality"
    return a == b
    
def is_zero(x):
    "Checks whether x is 0"
    return x == 0
    
def sub1(x):
    "Subtracts 1 from x"
    return x-1
    
def add1(x):
    "Adds 1 to x"
    return x+1
    
#----{ Chapter 1 }-----------------------------------------------------
def is_atom(x):
    "Checks if 'x' is an atom"
    return not is_null(x) and not is_pair(x)

#----{ Chapter 2 }-----------------------------------------------------
def is_lat(l):
    "Checks to see if l is a list of atoms"
    if is_null(l):
        return True
    elif is_atom(car(l)):
        return is_lat(cdr(l))
    else:
        return False
        
def is_member(a, lat):
    "Checks if a is a member of lat"
    if is_null(l):
        return True
    else:
        return is_equal(a, car(lat)) or is_member(a, cdr(lat))
        
#----{ Chapter 3 }-----------------------------------------------------
def rember(s, lat):
    "Removes first instance of member a from lat"
    if is_null(lat):
        return []
    elif is_equal(car(lat), s):
        return cdr(lat)
    else:
        return cons(car(lat), rember(a, cdr(lat)))
        
def multi_rember(a lat):
    "Removes all instances of member a from lat"
    if is_null(lat):
        return []
    elif is_equal(car(lat), a):
        return multirember(a, cdr(lat))
    else:
        return cons(car(lat), multi_rember(a, cdr(lat)))
        
def firsts(l):
    "Gets the first of every list in l"
    if is_null(l):
        return []
    else:
        return cons( car( car(l) ), firsts( cdr(l) ) )
        
def insertR(new, old, lat):
    "Inserts 'new' to the right of the first instance of 'old' in lat"
    if is_null(lat):
        return []
    elif is_equal(old, car(lat)):
        return cons( car(lat), cons(new, cdr(lat)) )
        
def multi_insertR(new, old, lat):
    "Inserts 'new' to the right of every instance of 'old' in lat"
    if is_null(lat):
        return []
    elif equal(old, car(lat)):
        return cons( car(lat), cons( new, multi_insertR(new, old, cdr(lat)) ) )
    else:
        return cons( car(lat), multi_insertR(new, old, cdr(lat)) )
        
def insertL(new, old, lat):
    "Inserts 'new' to the left of the first instance of 'old' in lat"
    if is_null(lat):
        return []
    elif is_equal(old, car(lat)):
        return cons(new, lat)
    else:
        return cons( car(lat), insertL(new, old, cdr(lat)) )
        
def multi_insertL(new, old, lat):
    "Inserts 'new' to the left of every instance of 'old' in lat"
    if is_null(lat):
        return []
    elif is_equal(old, car(lat)):
        return cons( new, cons(old, multi_insertR(new, old, cdr(lat))) )
    else:
        return cons( car(lat), multi_insertL(new, old, cdr(lat)) )
        
def subst(new, old, lat):
    "Replaces first instance of 'old' with 'new'"
    if is_null(lat):
        return []
    elif is_equal(old, car(lat)):
        return cons(new, cdr(lat))
    else:
        cons( car(lat), subst(new, old, cdr(lat)) )
        
def subst2(new, o1, o2, lat):
    "Replaces first instance of either 'o1' or 'o2' with 'new'"
    if is_null(lat):
        return []
    elif is_equal(o1, car(lat)) or is_equal(o2, car(lat)):
        return cons(new, cdr(lat))
    else:
        cons( car(lat), subst2(new, o1, o2, cdr(lat)) )
        
def multisubst(new, old, lat):
    "Replaces all instances of 'old' with 'new'"
    if is_null(lat):
        return []
    elif is_equal(old, car(lat)):
        return cons( new, multisubst(new, old, cdr(lat)) )
    else:
        return cons( car(lat), multisubst(new, old, cdr(lat)) )
        
#----{ Chapter 4 }-----------------------------------------------------
def plus(n, m):
    "Adds 'n' to 'm'"
    if is_zero(m):
        return 0
    else:
        return add1( plus(n, sub1(m)) )
        
def minus(n, m):
    "Subtracts 'm' from 'n'"
    if is_zero(m):
        return 0
    else:
        return sub1( minus(n, sub1(m)) )

def mult(n, m):
    "Multiplies 'n' by 'm'"
    if is_zero(m):
        return 0
    else:
        return plus( n, mult(n, sub1(m)) )
        
