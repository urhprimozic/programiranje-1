from random import randint, setstate
from tqdm import tqdm
# ideja: kot fisher -yates, samo da se na koncu
# ustavi, če mamo že sodo transpozicij

# (ko prideš nazaj iz laufa)


def soda(n: int):
    """
    Vrne sodo permutacijo n števil. Vse s o enako verjetne.

    Časovna zahtevnost
    ---------------
    n - iteracij, vsako iteracijo zamenjam 2 (ali pa sploh nič)
    --> 0(n)

    Prostorska
    ----------
    Vse se dogaja na tabeli dolžine n
    --> O(n)

    Dokaz za verjetnost P(soda(n) == [a1, ..., an]
    --------------------------------------------
    ### soda(n) je res soda
        Vemo, da je soda natanko tista ki ima sodo število (pravih) transpozicij
        V algoritmu skos delamo tranzpozicije (ali pa identiteto)
        (torej mamo kompozitum tranzpozicij (lihe) in id(soda)
        če poskrbimo, da je sodo št. tranzpozicij (poskrbimo), smo safe
    ### Ostalo je očitno isto ko pr fisher yatsju

    """
    particija = [i for i in range(1, n+1)]

    if n==1:
        return particija
    

    def zamenjaj(i, j):
        # res dela
        particija[i], particija[j] = particija[j], particija[i]

    st_transpozicij = 0
    # i - meja. Menjamo le elemente z indeksom < i
    for i in range(n-1, 1, -1):
        j = randint(0, i)
        zamenjaj(i, j)
        
        if i != j:
            st_transpozicij += 1
    if st_transpozicij % 2 == 1:
        zamenjaj(0, 1)

    return particija

def test_na_3(n):
    legal = {(1,2,3) : 0, (3,1,2): 0, (2,3,1) : 0}
    for i in tqdm(range(n), total=n):
        test = tuple(soda(3))
        if legal.get(test) is None:
            print(f'Ilegalna permutacija {test}!!')
            return False 
        legal[test] += 1
    ans = [f'Permutacija {l} : stevilo: {legal[l]}, P={(legal[l]/n)*100}%' for l in list(legal.keys())]
    for l in list(legal.keys()):
        print(f'Permutacija {l} : stevilo: {legal[l]}, P={(legal[l]/n)*100}%' )
    return ans
