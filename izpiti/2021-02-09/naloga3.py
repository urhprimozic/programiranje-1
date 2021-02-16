from functools import lru_cache

# =============================================================================
# Psička Nara po njivi preganja krokarje. Opazila je, da jo lastnik čaka na
# drugem koncu polja, zato hiti k njemu, pri tem pa hoče prestrašiti kar se da
# veliko ubogih ptičev.
#
# Njivo predstavimo kot matriko, ki v vsakem polju vsebuje število krokarjev,
# ki jih pasja navihanka prežene, če teče preko tega polja.
# =============================================================================

primer = [
    [2, 3, 0, 2, 9],
    [8, 3, 5, 1, 2],
    [1, 2, 7, 2, 0],
    [4, 3, 6, 5, 5],
]
nicle = [[0,0,0,0,0],
[0,0,0,0,0],
[0,0,0,0,0],
[0,0,0,0,0],
[0,0,0,0,0]]
# (a)
# =============================================================================
# Nara se nahaja v zgornjem levem kotu njive (polje `(0, 0)`). Ker se ji mudi
# k lastniku, se vztrajno premika desno. Na vsakem koraku se lahko premakne:
#   - desno
#   - diagonalno desno-gor
#   - diagonalno desno-dol
#
# Pregon krokarjev zaključi na poljubnem skrajno desnem polju njive. Napišite
# funkcijo, ki izračuna največje število krokarjev, ki jih lahko nagajivka
# prežene.
# =============================================================================
def st_krokarjev(polje):
    visina = len(polje)
    if visina == 0:
        return 0
    sirina = len(polje[0])

    @lru_cache(maxsize=None)
    def f(vrstica, stolpec):
        # če pridemo ven iz matrike:
        if vrstica < 0 or vrstica >= visina:
            return -100
        # če smo v zadnjem stolpcu
        if stolpec == sirina -1:
            return polje[vrstica][stolpec]
        maxi = -float('inf')
        for dy in [-1, 0, 1]:
            maxi = max(maxi, polje[vrstica][stolpec] + f(vrstica + dy, stolpec + 1))
        return maxi
    
    return f(0, 0)


# (b)
# =============================================================================
# Funkcijo iz točke (a) prilagodite tako, da ji dodatno podate indeks vrstice,
# v kateri Nara začne, in indeks vrstice, v kateri Nara konča.
#
# Funkcija naj vrne seznam VSEH optimalnih poti, kjer pot predstavimo s
# seznamom indeksov polj, preko katerih Nara teče.
# =============================================================================
def vse_poti(polje, zacetek, konec):
    visina = len(polje)
    if visina == 0:
        return 0
    sirina = len(polje[0])
    
    memo = {}

    def f(vrstica, stolpec):
        # memoizacija:
        if not (memo.get((vrstica, stolpec)) is None):
            return memo[(vrstica, stolpec)]
        # če pridemo ven iz matrike:
        if vrstica < 0 or vrstica >= visina:
            memo[(vrstica, stolpec)] = None
            return None # če je ta pot neveljavna, vrnem None
        # če smo v zadnjem stolpcu
        if stolpec == sirina -1:
            if vrstica != konec:
                return None
            # drugače vračam par 
            # (vrednost,  seznam poti iz tukaj do konca )
            memo[(vrstica, stolpec)] = (polje[vrstica][stolpec],  [[(vrstica, stolpec)]] )
            return memo[(vrstica, stolpec)]

        # drugač vseen sprobamo vse poti    
        max_pot = -float('inf')
        vsa_nadaljevanja = []
        for dy in [-1, 0, 1]:
            ans = f(vrstica + dy, stolpec + 1)
            if ans is None:
                continue 
            vrednost_poti, nadaljevanje = ans
            vrednost_poti = vrednost_poti + polje[vrstica][stolpec]
            if vrednost_poti  < max_pot:
                continue # TOLE DA SLABŠO POT
            if vrednost_poti > max_pot: #ne vzet prejšne poti
                vsa_nadaljevanja = nadaljevanje 
                max_pot = vsa_nadaljevanja
            # drugače pa sta to dve optimalni poti
            # max pot se ne spremeni
            vsa_nadaljevanja = vsa_nadaljevanja + nadaljevanje
        
        if max_pot == -float('inf'):
            return None
        # dodaš ta index na poti
        ans = [] 
        for pot in vsa_nadaljevanja:
            ans.append([(vrstica, stolpec)] + pot)

        memo[(vrstica, stolpec)] = (max_pot, vsa_nadaljevanja)
        return (max_pot, ans)
    

    return f(zacetek, 0)