# Elaborato

Scrivere un programma Python che legge il file "primaryschool.tsv" a questo link.  
Il file originale si trova sul sito sociopatterns al link: http://www.sociopatterns.org/datasets/primary-school-temporal-network-data/  

I dati corrispondono a una serie di contatti tra bambini o bambini e insegnanti in una scuola primaria. Il dataset è stato usato nella pubblicazione BMC Infectious Diseases 2014, 14:695.  
Ogni riga del file è una quintupla i cui campi sono separati dal carattere di tabulazione:
t   i    j    r        s  
dove  
    - t è il tempo in cui avviene un contatto e in particolare il contatto corrisponde a 20 secondi di contatto avvenuto nell'intervallo [t-20s; t]  
    - i è l'id di un alunno o di un insegnante  
    - j è l'id di un alunno o di un insegnante  
    - r è la classe dell'alunno i oppure il valore "teacher" se i è un insegnante  
    - s è la classe dell'alunno j oppure il valore "teacher" se j è un insegnante

Per esempio la riga:  
32060   1653    1837    Teachers        4A  
indica che dal tempo 32060-20=32040 al tempo 32060 l'insegnante 1653 ha incontrato il bambino 1837 della 4A.  
Le righe sono ordinate in ordine di tempo.  
Le categorie/classi sono 11: ['1A','1B','2A','2B','3A','3B','4A','4B','5A','5B','Teachers']

---
## Parte A
**Realizzare un'animazione usando il modulo turtle simile a quella nel video.**

In particolare ad ogni individuo, bambino o insegnante, è associata una tartaruga.  
Ogni categoria ha un colore diverso e le tartarughe nella stessa categoria sono vicine.  
Nell'animazione ad esempio abbiamo diviso l'area in 11 rettangoli e ogni categoria sta nel suo rettangolo.

L'animazione deve procedere in due fasi:

A. La prima fase legge i dati e posiziona in modo casuale le tartarughe, facendo in modo che le tartarughe della stessa categoria (ovvero classe 1A, 1B, ... , Teacher) siano vicine.
B. La seconda fase rilegge i dati e per ogni riga fa effettuare il movimento. In particolare:  
  Per ogni riga, se la riga è "t i j r s" significa che la tartaruga con id i dalla sua posizione deve andare nella posizione della tartaruga con id j e tornare al suo posto, ovvero nella sua posizione iniziale. 

Per esempio nella riga dell'esempio sopra l'insegnante con l'id 1653 va nella posizione del bambino 1837 (che si trova nella classe delle tartarughe di 4A) e torna a posto.  
Nel fare questo nell'animazione di esempio, l'insegnante, che ha colore rosso, lascia una striscia di colore rosso mentre si muove e torna a posto. 

Nella prima fase usare la funzione penup() per le tartarughe per posizionarle nella loro posizione casuale senza lasciare una traccia.  
Nella seconda fase usare la funzione pendown() per far lasciare una traccia alle tartarughe del loro movimento.  
E' consigliato l'uso di Pandas per leggere/manipolare i dati.

Interrompere l'animazione dopo la lettura di y righe con y impostabile a priori.

### SUGGERIMENTI

Per ogni tartaruga x usare le seguenti funzioni del modulo turtle:

* t= turtle.Turtle()
* t.color( y ) per qualche colore y, ad esempio 'light salmon','purple','pink','violet','yellow','gray','olive','orange','blue','green','red'
* t.penup()
* t.speed(100)
* t.goto(x,y) per qualche x,y
* t.pendown()
* t.pos() restituisce la posizione della tartaruga t

---
## Parte B
**Scrivere un programma python per risolvere ciascuna delle domande specificando e motivando la complessità (nel codice e nella relazione).**

A. L'individuo che ha avuto più interazioni
B. L'individuo che ha avuto più interazioni con individui diversi (ad esempio, se ha interagito con lo stesso bambino due volte conta 1; anche se ha interagito due volte con lo stesso insegnante conta 1)
C. L'individuo che ha avuto più interazioni con categorie diverse (ad esempio, se ha interagito con due bambini della 1A, conta 1; anche se ha interagito con due insegnanti conta 1)
D. La coppia di individui che hanno interagito più volte
E. La coppia di classi che hanno avuto più interazioni, dove ogni interazione fatta da x e y appartenenti a classi diverse conta 1.
F. Usando Matplotlib, fare il grafico del numero di interazioni al passare del tempo all'interno di ciascuna classe (inclusa la classe degli insegnanti)
G. Considerando come 1 il tempo di incubazione, se un individuo prende l'influenza al tempo x, quali sono gli individui che sono a rischio?

Per l'ultima domanda scrivere una procedura che prende in input l'id dell'individuo infetto e restituisce l'elenco degli infettati.  
Lanciare la procedura con random id (sensati) e ripetere calcolando la media degli infetti.  

**SUGGERIMENTO:**
- Se x ha l'influenza e incontra y al tempo t, ogni contatto di y dopo il tempo t è a rischio.  
- Se z ha incontrato y al tempo t+1, anche z è a rischio.  
- Se z ha incontrato r al tempo t+2, anche r è a rischio, ecc...  
- Per cui se l'elenco dei contatti è questo e x è malato, y,z,r sono a rischio  
  x y 10  
  z y 11  
  z r 12  
- Se però l'elenco è questo e x è malato, solo y è a rischio perchè il contatto che y ha avuto con z non ha aspettato il tempo di incubazione.  
  x y 10  
  z y 10  
  z r 12  

**La relazione deve contenere una breve spiegazione di quanto fatto e deve riportare la complessità per ciascuna procedura sviluppata, motivando il risultato e specificando se si tratta di analisi nel caso medio o nel caso pessimo.**