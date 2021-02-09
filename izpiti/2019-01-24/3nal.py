from functools import lru_cache
#žaba
def st_skokov(mocvirje):
    k= len(mocvirje)
    @lru_cache(maxsize=None)
    def f(i, energija):
        if i >= k:
            return 0 # zuni smo
        # drugač maš pa opcije:
        # skočit na vsa polja med 1 in energija
        ans = float('inf')
        for dolzina in range(1, energija + mocvirje[i] + 1):
            ans = min(ans, 1+f(i + dolzina, energija - dolzina + mocvirje[i]))
        return ans
    return f(0, 0)