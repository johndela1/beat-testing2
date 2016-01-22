from math import sin

def f(x, k):
    if x < 0:
        return sin(-8*x)/ (8*x)
    elif x >=0:
        return 2*x+ 9*k -7
    else:
        raise ValueError

if __name__ == '__main__':
    k = 1
    while k <= 1:
        deltas = [f(x+1, k)-f(x,k) for x in range(9,11)]
        print deltas
        if deltas[0] == deltas[1]:
            print ('k', k,deltas)
        #print deltas
        #print k,(f(7, k) + 1) - (f(8,k))
       #     print 'k: ', k, 'delta:', delta
        #print k,(f(9, k) + 1) - (f(10,k))
#    print (f(8, k) + 1) - (f(9,k))
        # print('8: ', f(8, k), k), '9: ',(f(9, k), k)
        k+=.25

