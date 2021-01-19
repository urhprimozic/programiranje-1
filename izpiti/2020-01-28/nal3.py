from functools import lru_cache
# ni važno katero korito je kje
# le preštet morš vse pozicije


@lru_cache(maxsize=None)
def st_razlicnih_postavitev(n: int, m: int, l: int):
    """
    n - širina balkona

    m - število korit

    l - širina nageljnov
    """
    if m==0: 
        return 1 
    if n < l:
        return 0
    # drugače
    stevilo= 0
    # zacetek j polje kjer postavmo
    for zacetek in range(0, n - l+1):
        stevilo += st_razlicnih_postavitev(n - 1 - zacetek -l, m-1, l)
    return stevilo


def kapitalizem(n, sirine):
        m = len(sirine)

        @lru_cache(maxsize=None)
        def f(n, i):
            if i >= m:
                return 1

            l = sirine[i]

            if n < l:
                return 0
            stevilo = 0
            for zacetek in range(0, n - l+1):
                stevilo += f(n -1 -zacetek - l, i+1)
            return stevilo
        return f(n,0)
    