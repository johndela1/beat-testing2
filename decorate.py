def jj_strip(func):
    def decorated_function(*args, **kwargs):
        return  func(args[0].strip())

    return decorated_function
@jj_strip
def foo(s):
    print 'hey from foo!'
    return s

s = '     hey         '
print '->'+s+'<-'
print '->'+foo(s)+'<-'
