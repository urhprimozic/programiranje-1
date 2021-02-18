import random

def fisher(n):
    ans = [i for i in range(1, n+1)]
    for i in range(n-1, 0, -1):
        j = random.randint(0, i)
        ans[i], ans[j] = ans[j], ans[i]
    return ans   

def memoiziraj(f):
    memo = {}
    def mem_f(x):
        if not memo.get(x) is None:
            return memo[x]
        memo[x] = f(x)
        return memo[x] 
    return mem_f 

[1,4,2,6,9,6,4,5,8]

