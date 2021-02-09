from functools import lru_cache
# =============================================================================
# Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
# gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
# Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
# fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
# prve fotografije in lokacijo druge fotografije kar se da velik vzpon.
#
# Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
# vrstnem redu, kot si sledijo po poti. Na primer:
#
#    [350, 230, 370, 920, 620, 80, 520, 780, 630]
#
# V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
# in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
# Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
# razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
# =============================================================================

# (a)
# -----------------------------------------------------------------------------
# Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
# višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
# utemeljite v komentarju.
# -----------------------------------------------------------------------------
"""
ideja:

ko si na enem vrhu, lahko:
    -slikaš prvo sliko na njem in poiščeš drug vrh, kjer narediš drugo sliko (o(n))
    - se premakneš na drug vrh , in tam nardiš prvo sliko


"""
test = [350, 230, 370, 920, 620, 80, 520, 780, 630]

def najvecja_razlika(vrhovi):
    st_vrhov = len(vrhovi)
    @lru_cache(maxsize=None)
    def f(index):
        """
        f (i) je najvecja razlika možna, če zaštartamo na i-tem vrhu
        """
        if index >= st_vrhov:
            # to je varnsostno ko pridemo na konec tabele
            return -10000 #če smo na koncu tabele je to slab premik
       
        #lahko naredimo prvo sliko tukej:
        # torej bo drugi vrh ta ka da bo od tukej najvecja razlika
        diff = max([vrhovi[k] - vrhovi[index] for k in range(index, st_vrhov)])
        
        # lahko pa naredimo prvo sliko nekje drugje
        for j in range(index + 1, st_vrhov):
            diff = max(diff, f(j))
        return diff 
    return f(0)

# ne glej tega to ni u
def pocasna_najvecja_razlika(vrhovi):
    """
    ni O(nlogn)


    """

    st_vrhov = len(vrhovi)
    @lru_cache(maxsize=None)
    def f(index):
        """
        f (i) je najvecja razlika možna, če zaštartamo na i-tem vrhu
        """
        if index >= st_vrhov:
            # to je varnsostno ko pridemo na konec tabele
            return -10000 #če smo na koncu tabele je to slab premik
        maxi = 0 # v štartu je ena možna razlika da ne gremo nikamor (premaga tisto izven tabele)
        
        # sprobamo če naredimo drugo sliko na kateremu od naslednih vrhov
        for j in range(index + 1, st_vrhov):
            maxi = max(maxi, f(j) + (vrhovi[j] - vrhovi[index]))
        return maxi 
    return max([f(i) for i in range(0, st_vrhov)])



# (b)
# -----------------------------------------------------------------------------
# Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
# tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
# -----------------------------------------------------------------------------
"ideja: f namest razlike vrača indexa začetka pa konca "