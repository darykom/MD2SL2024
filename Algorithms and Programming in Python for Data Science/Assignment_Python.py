# -*- coding: utf-8 -*-
"""
Created on Mon Sep  9 00:06:11 2024

@author: dario
"""

import turtle
import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
from matplotlib import colormaps

#savePlot = False # per salvare su file grafici (da inserire nella relazione)

#------------------------------------------------------------------------------

class Viewer:
    
    # margine per non disegnare tartarughe troppo vicine ai bordi dei propri 
    # settori, espresso come variabile di classe
    Margin = 15
    
    def __init__(self, p_affiliations, p_interactions):
        
        self.Affiliations = p_affiliations
        self.set_Screen()
        self.create_Affiliation_Colors()
        self.create_Turtle_Layout(p_interactions)
        
        
    def set_Screen(self):
        self.Wn = turtle.Screen()
        self.Wn.bgcolor('black')
        self.Height = self.Wn.window_height()
        self.Width = self.Wn.window_width()
        self.Step_x = self.Width/len(self.Affiliations)

    def create_Affiliation_Colors(self):
        palette = colormaps['Spectral']
        A = len(self.Affiliations)
        self.Colors = {}
        for a in range(A):
            key_a = self.Affiliations[a]
            rgba = palette(a/(A-1))
            self.Colors[key_a] = rgba[0:3]
            
    def create_Turtle_Layout(self, p_interactions):
        print('Posizionamento delle tartarughe')
        
        school = self.create_School_Dataframe(p_interactions)
        
        self.Turtles = {} #dizionario vuoto
        for _, row in school.iterrows():
            s = row.subject
            aff = row.affiliation
            self.Turtles[s] = self.set_Turtle(aff)
            
            
    def create_School_Dataframe(self, p_dfInteractions):
        # isolo le colonne i e r, rinominandole rispet#tivamente come subject e affiliation
        df_ir = (p_dfInteractions[['i', 'r']]).rename(columns={'i':'subject', 'r':'affiliation'}).drop_duplicates()
        # isolo le colonne j e s, rinominandole rispettivamente come subject e affiliation
        df_js = (p_dfInteractions[['j', 's']]).rename(columns={'j':'subject', 's':'affiliation'}).drop_duplicates()
        # impilo i due dataframe e rimuovo i duplicati
        df = pd.concat([df_ir, df_js]).drop_duplicates()
        # ordino rispetto alle classi, per il successivo posizionamento per classi delle tartarughe
        return df.sort_values(by='affiliation')


    def set_Turtle(self, p_affiliation):
        position = self.set_Turtle_Position(p_affiliation)
        
        t = turtle.Turtle()
        t.hideturtle()
        t.color(self.Colors[p_affiliation])
        t.speed(0) #velocità max.
        t.penup()
        t.goto(position)
        t.pendown()
        t.showturtle()
        return t

    def set_Turtle_Position(self, p_affiliation):

        x = Viewer.Margin + (self.Step_x-2*Viewer.Margin)*np.random.uniform()
        y = Viewer.Margin + (self.Height-2*Viewer.Margin)*np.random.uniform()

        id_aff = self.Affiliations.index(p_affiliation)
        x_offset = self.Step_x * id_aff - self.Width/2;

        return np.round((x+x_offset, y-self.Height/2), 0)


    def play(self, p_interactions, p_N):
        print('Animazione in corso')
        N = min(p_N, p_interactions.shape[0])
        for n in range(N):
            id_i = p_interactions.loc[n,'i']
            id_j = p_interactions.loc[n,'j']
            
            src = self.Turtles[id_i].pos()
            dst = self.Turtles[id_j].pos()

            self.Turtles[id_i].goto(dst)
            self.Turtles[id_i].goto(src)
        
        #print('Cliccare sulla finestra dell\'animazione per chiudere')
        #turtle.exitonclick()
        print('Cliccare sulla X dell\'animazione per chiudere')
        turtle.done()
        turtle.bye()

#------------------------------------------------------------------------------
# 1. L'individuo che ha avuto più interazioni
def answer1(p_meetings): #O(n)
    argmax = set() #O(1), set vuoto
    maxValue = 0 #O(1)
    counts = {} #O(1), dizionario vuoto
    
    for _, row in p_meetings.iterrows(): # cicla n volte su costi O(1) -> O(n)
    
        for p in [row.i, row.j]: # cicla 2 volte su costi O(1) -> O(1)
            if p in counts: #O(1)
                counts[p] += 1 #O(1)
            else:
                counts[p] = 1 #O(1)        
            if maxValue <= counts[p]: #O(1)
                if maxValue < counts[p]: #O(1)
                    #resetto tutto 
                    argmax = {p} #O(1)
                    maxValue = counts[p]
                else:
                    argmax.add(p) #O(1)
    
    print('Domanda 1: argmax(' + str(maxValue) + ') = ' + str(argmax))
    
# 2. L'individuo che ha avuto più interazioni con individui diversi (ad esempio, 
#    se ha interagito con lo stesso bambino due volte conta 1; anche se ha 
#    interagito due volte con lo stesso insegnante conta 1)
def answer2(p_meetings): #O(n)
    argmax = set() #O(1)
    maxValue = 0 #O(1)
    counts = {} #O(1)
    meets = {} #O(1)
    for _, row in p_meetings.iterrows(): # cicla N volte su costi O(1) -> O(n)
    
        for p,q in [(row.i,row.j), (row.j,row.i)]: # cicla 2 volte su costi O(1) -> O(1)
            if p not in counts: #O(1)
                counts[p] = 1 #O(1)
                meets[p] = {q} #O(1), creo set con solo q
            else: 
                if q not in meets[p]: #O(1)
                    meets[p].add(q) #O(1)
                    counts[p] += 1 #O(1)
                
            if maxValue <= counts[p]: #O(1)
                if maxValue < counts[p]: #O(1)
                    #resetto tutto 
                    argmax = {p} #O(1)
                    maxValue = counts[p]
                else:
                    argmax.add(p) #O(1)
    
    print('Domanda 2: argmax(' + str(maxValue) + ') = ' + str(argmax))
    
    
# 3. L'individuo che ha avuto più interazioni con categorie diverse (ad 
#    esempio, se ha interagito con due bambini della 1A, conta 1; anche se ha 
#    interagito con due insegnanti conta 1)
def answer3(p_meetings): #O(n)
    argmax = set() #O(1)
    maxValue = 0 #O(1)
    counts = {}  #O(1)
    c_meets = {}#O(1)
    
    for _, row in p_meetings.iterrows(): # cicla n volte su costi O(1) -> O(n)
    
        for p,cq in [(row.i,row.s), (row.j,row.r)]: # cicla 2 volte su costi O(1) -> O(1)
        
            if p not in counts: #O(1)
                counts[p] = 1 #O(1)
                c_meets[p] = {cq} #O(1)
            elif cq not in c_meets[p]: #O(1)
                    c_meets[p].add(cq) #O(1)
                    counts[p] += 1 #O(1)
                
            if maxValue <= counts[p]: #O(1)
                if maxValue < counts[p]: #O(1)
                    #resetto tutto 
                    argmax = {p} #O(1)
                    maxValue = counts[p] #O(1)
                else:
                    argmax.add(p) #O(1)
                    
    print('Domanda 3: argmax(' + str(maxValue) + ') = ' + str(argmax))
    
    
# 4. La coppia di individui che hanno interagito più volte
def answer4(p_meetings): #O(n)
    argmax = set() #O(1)
    maxValue = 0 #O(1)
    counts = {} #O(1)
    for _, row in p_meetings.iterrows(): # cicla N volte su costi O(1)

        #pair = (min(row.i, row.j), max(row.i, row.j))
        if row.i < row.j: #O(1)
            pair = (row.i, row.j) #O(1)
        else: #O(1)
            pair = (row.j, row.i) #O(1)
            
        if pair in counts: #O(1)
            counts[pair] += 1 #O(1)
        else:
            counts[pair] = 1 #O(1)
            
        if maxValue <= counts[pair]: #O(1)
            if maxValue < counts[pair]: #O(1)
                #resetto tutto 
                argmax = {pair} #O(1)
                maxValue = counts[pair] #O(1)
            else:
                argmax.add(pair) #O(1)
                
    print('Domanda 4: argmax(' + str(maxValue) + ') = ' + str(argmax))
    
# 5. La coppia di classi che hanno avuto più interazioni, dove ogni interazione
#    fatta da x e y appartenenti a classi diverse conta 1.
def answer5(p_meetings): #O(n)
    argmax = set()
    maxValue = 0
    counts = {} 
    meets = {} 
    for _, row in p_meetings.iterrows(): # cicla N volte su costi O(1)

        if (row.r != row.s): #O(1)
            #pair_c = (min(row.r, row.s), max(row.r, row.s))
            if row.r < row.s: #O(1)
                pair_c = (row.r, row.s) #O(1)
            else: #O(1)
                pair_c = (row.s, row.r) #O(1)
                
            #pair_p = (min(row.i, row.j), max(row.i, row.j))
            if row.i < row.j: #O(1)
                pair_p = (row.i, row.j) #O(1)
            else: #O(1)
                pair_p = (row.j, row.i) #O(1)
                
            if pair_c in counts: #O(1)
                if pair_p not in meets[pair_c]: #O(1)
                    counts[pair_c] += 1 #O(1)
                    meets[pair_c].add(pair_p) #O(1)
            else:
                counts[pair_c] = 1 #O(1)
                meets[pair_c] = {pair_p} #O(1) 
                
            if maxValue <= counts[pair_c]: #O(1)
                if maxValue < counts[pair_c]: #O(1)
                    #resetto tutto 
                    argmax = {pair_c} #O(1)
                    maxValue = counts[pair_c]
                else:
                    argmax.add(pair_c) #O(1)
                    
    print('Domanda 5: argmax(' + str(maxValue) + ') = ' + str(argmax))
    
# 6. Usando Matplotlib, fare il grafico del numero di interazioni al passare 
#    del tempo all'interno di ciascuna classe (inclusa la classe degli 
#    insegnanti)
def answer6(p_meetings, p_categories, p_tSlot): #O(n)
    time = [] #O(1)
    memoCounts = {} #O(1)
    totCounts = {} #O(1)
    counts = {} #O(1)
    for cat in p_categories: #cicla 11 volte su costi O(1) -> O(1)
        counts[cat] = 0 #O(1)
        memoCounts[cat] = 0 #O(1)
        totCounts[cat] = [] #O(1)
        
        
    tLast = p_meetings.loc[0,'t'] ##O(1)
    for _, row in p_meetings.iterrows(): # cicla n volte su costi O(1) -> O(n)
        
        tCur = row.t  #O(1)
        
        if tLast != tCur: #O(1)
            time.append(tLast-p_tSlot) #O(1)
            for cat in p_categories: #cicla 11 volte su costi O(1) -> O(1)
                memoCounts[cat] += counts[cat] #O(1)
                totCounts[cat].append(memoCounts[cat]) #O(1)
                counts[cat] = 0 #O(1)
            tLast = tCur #O(1)
            
        if row.r == row.s: #O(1)
            counts[row.r] += 1 #O(1)
    
    # svuoto i valori dell'ultimo slot temporale
    time.append(tLast-p_tSlot) #O(1)
    for cat in p_categories: #cicla 11 volte su costi O(1) -> O(1)
        memoCounts[cat] += counts[cat] #O(1)
        totCounts[cat].append(memoCounts[cat]) #O(1)

            
    fig, ax = plt.subplots(figsize=(12, 5.3))
    for cat in p_categories:
        ax.plot(time, totCounts[cat], label=cat)

    ax.set_xlabel('Time')
    ax.set_ylabel('Interactions')
    ax.set_title('Trends for internal class interactions')
    ax.grid(color='gainsboro', linestyle=':', linewidth=1)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.legend()
    #if savePlot: plt.savefig('./LaTex/Figure/InternalClassInteraction.pdf', bbox_inches='tight')
    plt.show()
    print('Domanda 6: grafico creato')
    
def answer7(p_meetings, p_tSlot, p_tInc, p_Tavg): #O(n)
    
    print('Domanda 7: in progress')
    
    people = [] #O(1)
    for _, row in p_meetings.iterrows(): # cicla n volte su costi O(1) -> O(n)
        if row.i not in people:
            people.append(row.i) #O(1)
        if row.j not in people:
            people.append(row.j) #O(1)


            
    if (p_Tavg>0):
        sampled = np.random.randint(0, high=len(people), size=p_Tavg)
    else:
        print('Stima deterministica, ciclando su tutte le persone come paziente zero')
        p_Tavg = len(people)
        sampled = range(p_Tavg)
        
    avg = 0 #O(1)
    counts = [0] * p_Tavg #O(p_Tavg)
    
    for k in range(p_Tavg): #O(p_Tavg)
        trial = sampled[k] #O(1)
        risky = atRisk(p_meetings, people[trial],0, p_tSlot, p_tInc) #O(n)
        counts[k] = len(risky) #O(1)
        avg += counts[k] #O(1)
        
        #O(1)
        print('trial #' + str(k+1) + '/' + str(p_Tavg) + ' (' + str(people[trial]) +  ') persone a rischio: ' + str(counts[k]))
    avg /= p_Tavg #O(1)
    print('Media: ' + str(avg)) #O(1)

    # grafico per debug    
    # import seaborn as sns
    # bns = np.arange(min(counts )-0.5, max(counts)+0.6, 1)
    # fig, ax = plt.subplots(figsize=(12, 9))
    # _ = sns.histplot(counts, bins=bns, ax=ax)
    # _= ax.grid(axis='y', color='white', linewidth=1)
    # _= ax.spines['top'].set_visible(False)
    # _= ax.spines['right'].set_visible(False)
    # _= ax.set_xlabel('At risk')
    # _= ax.set_ylabel('Counts')
    # _= ax.set_title('Flu propagation')

    

    
def atRisk(p_meetings, p_patient0, p_t0, p_tSlot, p_tInc): #O(n)
    #dizionario per annotare il tempo di contagio iniziale delle persone
    contactTime = {p_patient0: p_t0} #O(1)
    # lista delle persone a rischio
    riskyPeople = [] # O(1) 

    for _, row in p_meetings.iterrows(): # cicla n volte su  O(1) -> O(n)

        for p,q in [(row.i,row.j), (row.j,row.i)]: # O(1)
        
            # se p è persona esposta
            if p in contactTime: #O(1)
                # se p è contagioso
                if contactTime[p] + p_tInc <= row.t-p_tSlot: #O(1)
                    # se q non p stato esposto
                    if q not in contactTime:
                        contactTime[q] = row.t-p_tSlot #O(1)
                        # aggiungo direttamente q alla lista delle persone
                        # a rischio, pur non essendo passato il tempo di 
                        # incubazione, perché controllo dallo scarto temporale 
                        # con contactTime[q] la sua capacità di contagiare.
                        # Che stia incubando o sia potenzialmente contagiosa, 
                        # è comunque una persona a rischio
                        riskyPeople.append(q) #O(1)

    return riskyPeople 


#------------------------------------------------------------------------------

rstate = 3131
np.random.seed(rstate)

print('Caricamento delle informazioni')

categories = ('1A','1B', '2A','2B', '3A','3B', '4A','4B', '5A','5B', 'Teachers')

fname = 'primaryschool.tsv'
meetings = pd.read_table(fname, names = ('t','i','j','r','s'), sep='\t')

tSLot = 20 # finestra temporale d'interazione
tInc = 1 # tempo 'dincubazione
nTrials = 100 # numero di campioni per la domanda 7

anynull = meetings.isnull().values.any()
print('Null presenti in primaryschool? ' + str(anynull))

N = int(input("Numero di passi nell\'animazione?: "))
viewer = Viewer(categories, meetings)
viewer.play(meetings, N)

print('\nElaborazione domanda 1')
answer1(meetings)

print('\nElaborazione domanda 2')
answer2(meetings)

print('\nElaborazione domanda 3')
answer3(meetings)

print('\nElaborazione domanda 4')
answer4(meetings)

print('\nElaborazione domanda 5')
answer5(meetings)

print('\nElaborazione domanda 6')
answer6(meetings, categories, tSLot)

print('\nElaborazione domanda 7')
np.random.seed(rstate) #resetto il seed per avere coerenza coi dati nella relazione
answer7(meetings, tSLot, tInc, nTrials)

## extra, cicla 1 volta su tutti gli individui come paziente 0 (benchmark)
#answer7(meetings, tSLot, tInc, -1)

#-----------------------------------------------------------------------------