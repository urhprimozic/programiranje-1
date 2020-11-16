import csv
import os
import orodja
import requests
import re
###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'stran.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'podatki.csv'
# lepota
dir_name = os.path.dirname(__file__)

<<<<<<< HEAD
def download_url_to_string(url):  # passed 10:37
    """Funkcija kot argument sprejme niz in puskuša vrniti vsebino te spletne
=======
def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne
>>>>>>> dc12fa0fb17f7cac4f37ae832889eec2c2fd38a6
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print('Yo stari neki ni okj')
        print(f'pršlo je do napake številka {r.status_code}. Mal je dram')
    if r.status_code == requests.codes.ok:
        return r.text
    print('Napaka pri nalaganju urlja na string')
    return None


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    full_path = os.path.join(dir_name, directory, filename)
    with open(full_path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    vsebina = download_url_to_string(page)
    if vsebina:
        save_string_to_file(vsebina, directory, filename)
        return True
    return False


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
<<<<<<< HEAD
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz"""
    full_name = os.path.join(dir_name, directory, filename)
    with open(full_name, encoding='utf-8') as datoteka:
        return datoteka.read()
=======
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    raise NotImplementedError()
>>>>>>> dc12fa0fb17f7cac4f37ae832889eec2c2fd38a6


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
<<<<<<< HEAD
    """Funkcija poišče posamezne ogllase, ki se nahajajo v spletni strani in
    vrne njih seznam"""
    vzorec = (r'(?P<oglas><li class="EntityList-item EntityList-item--Regular[\S\s\n]+?</li>)'
    )
    ans = []
    for zadetek in re.finditer(vzorec, page_content):
        print(zadetek.groupdict().keys())
        ans.append(zadetek.groupdict().get('oglas'))
    return ans
=======
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne seznam oglasov."""
    raise NotImplementedError()

>>>>>>> dc12fa0fb17f7cac4f37ae832889eec2c2fd38a6

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
<<<<<<< HEAD
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke
    """
    # blok je str
    vzorec = (
        r'<h3 class="entity-title"><a name="\d+" class="link" href=".+?">.+?</a>'# dobi ven naslov
    #    r'[\S\s\n]+?data-src="(?P<url_slike>.+?)"' #dobi ven url thumbnaila
        #r'[\S\s\n]+?' #naprej do
        #r'<span class="entity-description-itemCaption">'#descriptiona
        #r''
    )
    ans = re.search(vzorec, block)
    return ans
=======
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke."""
    raise NotImplementedError()
>>>>>>> dc12fa0fb17f7cac4f37ae832889eec2c2fd38a6


# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    raise NotImplementedError()


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    slovarjev parametra ads enaki in je seznam ads neprazen."""
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    raise NotImplementedError()


# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran
    # if not save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename):
    #     print('lajf ni potica')
    #     return 'resnica'

    # Iz lokalne (html) datoteke preberemo podatke
<<<<<<< HEAD
    podatki = read_file_to_string(cat_directory, frontpage_filename)
    seznam_oglasov = page_to_ads(podatki)
    # Podatke prebermo v lepšo obliko (seznam slovarjev)
   # seznam_slovarjev = [get_dict_from_ad_block(
    #    block) for block in seznam_oglasov]
    # [{'ime' : 'Macek 1', cena: 69}, {}, ...]
    # Podatke shranimo v csv datoteko
   # write_cat_ads_to_csv(seznam_slovarjev, cat_directory, frontpage_filename)
    # Dodatno: S pomočjo parameteov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prense (četudi že obstaja)
=======

    # Podatke preberemo v lepšo obliko (seznam slovarjev)

    # Podatke shranimo v csv datoteko

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
>>>>>>> dc12fa0fb17f7cac4f37ae832889eec2c2fd38a6
    # in enako za pretvorbo

    #raise NotImplementedError()

podatki = read_file_to_string(cat_directory, frontpage_filename)
seznam_oglasov = page_to_ads(podatki)
for block in seznam_oglasov:
    gg = get_dict_from_ad_block(block)

if __name__ == '__main__':
    main()
