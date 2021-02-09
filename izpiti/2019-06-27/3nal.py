from functools import lru_cache
zabojniki = [1, 3, 4, 7, 10]

def st_nacinov(nosilnost):
    @lru_cache(maxsize=None)
    def f(nosilnost, index):
        if nosilnost < 0:
            return 0
        if nosilnost == 0:
            return 1 #veljaven način 
        # če smo na zandnjem indeksu
        if index == len(zabojniki) - 1:
            # endina šansa je , da probaš še enga notr stlačt
            return f(nosilnost - zabojniki[index], index)
        #drugač pa vzameš še enkga tazga pa probal
        # al pa vzameš večga pa rpobaš 
        # upoštevamo tud način , ko ga spustiš in vzameš večkrat 
        return (f(nosilnost - zabojniki[index], index) + # vzamemo malega in probamo vzet še enga malega
                f(nosilnost, index + 1) # ne vzamemo več nobenga tazga in gremo naprej 
                )
    return f(nosilnost, 0)

def resitve(nosilnost):
   # @lru_cache(maxsize=None)
    def f(nosilnost, index):
        '''
        Vrne seznam vseh izbir
        '''
        if nosilnost < 0:
            return None
        if nosilnost == 0:
            return [[]]
        # če smo na zandnjem indeksu
        a = None
        if index != len(zabojniki) - 1:
            a = f(nosilnost, index + 1)
        b = f(nosilnost - zabojniki[index], index)
        
        if a is None and b is None: 
            return None 
        ans = []
        if not a is None:
            for pot in a:# greš naprej, NE vzameš trenunte
                ans.append(pot)
        if not b is None:
            for pot in b:
                ans.append([zabojniki[index]] + pot)
        return ans
        

    return f(nosilnost, 0)