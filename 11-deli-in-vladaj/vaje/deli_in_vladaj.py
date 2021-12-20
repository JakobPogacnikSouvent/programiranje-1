###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end] (vključujoč oba robova).
#
# Primer: za [start = 1] in [end = 7] tabelo
#
#     [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 4 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
###############################################################################

def pivot(list, start, end):
    smaller_index = start+1
    bigger_index = end
    pivot = list[start] #pivot index is smaller_index - 1
    while smaller_index <= bigger_index:
        if list[smaller_index] < pivot:
            list[smaller_index-1], list[smaller_index] = list[smaller_index], list[smaller_index-1] # zamenjamo pivot z manjsim eltom na desni
            smaller_index += 1

        elif list[smaller_index] > pivot:
            if list[bigger_index] > pivot:
                bigger_index -= 1
            
            elif list[bigger_index] < pivot:
                list[smaller_index], list[bigger_index] = list[bigger_index], list[smaller_index]
    
    return smaller_index - 1

print("Pivot:")
a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
print(pivot(a, 1, 7))
b = [3, 2, 4, 5, 10, 11, 17, 15]
pivot(b, 0, 1)
print(b)
###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti element
# po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################

def kth_element_subfun(list, k, start, end):
    if start > end: return None

    k_th = pivot(list,start,end)
    if k_th == k:
        return list[k]
    elif k_th > k:
        return kth_element_subfun(list,k,start,k_th-1)
    elif k_th < k:
        return kth_element_subfun(list,k,k_th+1,end)
    
def kth_element(list,k):
    return kth_element_subfun(list,k,0,(len(list)-1))

print("kth:")
a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
print(kth_element(a,3))

###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort_part(a, start, end):
    if start <= end:
        n = pivot(a,start,end)
        quicksort_part(a,start,n-1)
        quicksort_part(a,n+1,end)

def quicksort(list):
    quicksort_part(list,0,len(a)-1)

print("Quicksort:")
a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
quicksort(a)
print(a)

###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
#
# Sestavite funkcijo [merge(target, list_1, list_2)], ki v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaka, naj bo prvi element iz prve tabele.
#
# Primer:
#
#     >>> list_1 = [1, 3, 5, 7, 10]
#     >>> list_2 = [1, 2, 3, 4, 5, 6, 7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> merge(target, list_1, list_2)
#     >>> target
#     [1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 10]
#
###############################################################################


def merge(target, list_1, list_2):
    index_1 = 0
    index_2 = 0
    target_index = 0
    while target_index < len(target):
        try:
            a = list_1[index_1]
            b = list_2[index_2]
            if a <= b:
                c = a
                index_1 += 1
            else:
                c = b
                index_2 += 1

        except IndexError: #index_1 or index_2 out of range
            try:
                c = list_1[index_1]
                index_1 += 1
            except IndexError:
                c = list_2[index_2]
                index_2 += 1
        
        target[target_index] = c
        target_index += 1


###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). Tabelo razdelimo na polovici,
# ju rekurzivno uredimo in nato zlijemo z uporabo funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja. Za
# razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je potrebno
# narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort(list):
    if len(list) > 1:
        n = len(list)//2
        a = []
        b = []
        for i in range(len(list)):
            if i < n:
                a.append(list[i])
            else:
                b.append(list[i])
            list[i] = -1

        merge(list, mergesort(a), mergesort(b))

    return list

print("Merge:")
a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
mergesort(a)
print(a)
