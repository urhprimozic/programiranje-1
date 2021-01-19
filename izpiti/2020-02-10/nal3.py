from functools import lru_cache

@lru_cache(maxsize=None)
def f(k : int, n : int, x=0):
    """
    Pove več, in sicer če se začne na x
    """
    if n <= 1: # zaporedje dolžine 1 ki se začne v x je lahko eno samo
        return 1
    ans = 0
    for y in range(x - k, x+k):# so vse možne nove lokacije
        if y < 0:
            continue #ilegalna lokacija
        ans += f(k, n-1, x=y) #narediš korak in prešteješ vse možnosti od tm
    return ans
    