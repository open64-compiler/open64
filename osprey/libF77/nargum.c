/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#include "cmplrs/host.h"
#include "comargs.h"
typedef uint32 *adresse;

int32 num_father_args, num_son_args;
uint32 *father_addr[34], father_len[34];
uint32 *son_addr[32], son_len[32];

int32 ctrace_[81];
#define PAQUOT	-1
#define QUOT	'\\'
#define DER_CAR	127
#define MAX_CAR 125

#define nb_arg_moi	num_son_args
#define nb_arg_pere	num_father_args
#define list_arg_pere	father_addr
#define list_arg_moi	son_addr
#define list_len_pere	father_len
#define list_len_moi	son_len

char *source, *destin;
extern void set_args(uint32 *, uint32 *);

#ifdef sgi
#define COPY_MOT(SRC,DEST)				\
	if (((ulong_t)DEST)&3 == 0)			\
	    *(uint32 *)(DEST) = *(uint32 *)(SRC);	\
	else {						\
	    register char *source = (char *) SRC,	\
			  *destin = (char *) DEST;	\
	    destin[0] = source[0];			\
	    destin[1] = source[1];			\
	    destin[2] = source[2];			\
	    destin[3] = source[3];			\
	}
#else
#define COPY_MOT(SRC,DEST)				\
	    source = (char *) SRC;			\
	    destin = (char *) DEST;			\
	    destin[0] = source[0];			\
	    destin[1] = source[1];			\
	    destin[2] = source[2];			\
	    destin[3] = source[3];
#endif

int32 *init_arg_()
{
	return(comargs__);
}


int32 nargum_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
   adresse *pere, *moi;
   int32 i, max;

   set_args(father_frame, (uint32 *) comargs__);

   max = num_father_args<num_son_args ? num_father_args : num_son_args - 1;

   pere = list_arg_pere;
   moi = &list_arg_moi[1];

   for (i=0; i<max; i++) {
       COPY_MOT(*pere, *moi)
       pere++; 
       moi++;
    }
   
   return(nb_arg_pere);
}


int32 nartab_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
   adresse *pere;
   int32 *imax;
   int32 *tab;
   int32 i, max;

   set_args(father_frame, (uint32 *) comargs__);

   imax = (int *)list_arg_moi[1];
   tab  = (int *)list_arg_moi[2];

   max = *imax;
   if (max > nb_arg_pere)
	max = nb_arg_pere;

    pere = list_arg_pere;

    for (i=0; i<max; i++) {
	COPY_MOT(*pere, tab)
	pere++;
	tab++;
    }
    return(nb_arg_pere);
}


void
tabarg_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
   adresse tab_pere;
   int32 *num;
   int32 *nbr;
   int32 *tab;
   int32 i, max;

   set_args(father_frame, (uint32 *) comargs__);

   num = (int *)list_arg_moi[1];
   nbr = (int *)list_arg_moi[2];
   tab = (int *)list_arg_moi[3];

   if (*num > nb_arg_pere || *num <= 0) return;

   max = *nbr;
   tab_pere = list_arg_pere[*num-1];
   for (i=0; i<max; i++) {
	COPY_MOT(tab_pere, tab);
	tab_pere++; tab++;
    }
}


void
rretrg_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
   int32 num, nbr;
   adresse tab, tab_pere;
   int32 i;

   set_args(father_frame, (uint32 *) comargs__);

   if (nb_arg_moi > 4 || nb_arg_moi < 3) return;

   num = *(list_arg_moi[1]);

   if (num<=0 || num>nb_arg_pere) return;

   if (nb_arg_moi ==3) {
	COPY_MOT(list_arg_moi[2], list_arg_pere[num-1])
    } else {
	nbr = *(list_arg_moi[2]);
	tab = list_arg_moi[3];
	tab_pere = list_arg_pere[num-1];
	for (i=0; i<nbr; i++) {
	    COPY_MOT(tab,tab_pere)
	    tab_pere++; tab++;
	}
    }
}


uint32 
rretvr_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
   int32 nbr, nini;
   adresse tab, *pere;
   int32 i, max;
   extern uint32 risc_return_ (uint32 *);

   set_args(father_frame, (uint32 *) comargs__);

   ctrace_[0]--;

   if (nb_arg_moi > 4 || nb_arg_moi < 3)
       return 0;

   if (nb_arg_moi==3) {
	risc_return_(list_arg_moi[2]);
	return(*list_arg_moi[2]);
    } else {
	nbr = *list_arg_moi[1];
	nini = *list_arg_moi[2] - 1;
	if (nini<0)
	    return 0;

	tab = list_arg_moi[3];
	pere = &list_arg_pere[nini];

	max = nb_arg_pere - nini;
	if (max > nbr) max = nbr;

	for (i=0; i<max; i++) {
	    COPY_MOT(tab,*pere)
	    pere++; tab++;
	}
    }
   return 0;
}


void
rretur_(uint32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
{
    ctrace_[0]--;
}



/*DBA le 16/12/89 modification pour transmission father_stack          */

/*----------------------------------------------------------------------
;
;	INTERFACES DE GESTIONS D'APPEL DE ROUTINES FORTRAN COMPORTANT
;		   UN NOMBRE D'ARGUMENTS VARIABLE DE TYPE CARACTERE
;-----------------------------------------------------------------------
;
;	REFERENCE DES TESTS : TSTCAR.FOR
;	REFERENCE DES FICHIERS RESULTAT : TSTCAR.DAT
;	PROGRAMME A ADAPTER : NARCAR.MAR
;----------------------------------------------------------------------*/

/********************************************************************
*  Principe: 
*   
*    - Une partie de programme qui est tres machine dependant "recup_arg"
*    a pour role de recuperer les argument d'appele et d'appelant. 
*    
*    - Les argument sont range de maniere suivant: 
*
*      *) nb_arg_moi:   Nombre d'argument d'appele.
*      *) nb_arg_pere:  Nombre d'argument d'appelant.
*
*      *) list_arg_moi : Un tableau qui contien les argument d'appele.
*      *) list_arg_pere: Un tableau qui contien les argument d'appelant.
*
*      *) list_len_moi : Un tableau qui contien la lengueur de chaque argument. 
*      *) list_len_pere: Un tableau qui contien la lengueur de chaque argument.
*
*    
*    -  Si une variable est de type chaine de caractere: Sa lengueur > 0.
*    -  Si une variable n'est pas de type chaine de caractere: Sa lengueur = 0.
*
*    -  Le nombre des argument est limite a 32.
*    
*                | 1er-arg |     | Len 1er-arg | 
*                |---------|     |-------------|
*                | Dernier |     | Len Dernier |
*                /   arg   /     /     arg     /
*                |---------|     |-------------|
*                |   0     |     |             |
*                |---------|     /             /
*                | Status  |     |             |
*                |---------|      -------------
*                |         |
*                /         /
*                |         |
*                 ---------         
*
*       Status:
*           - Un mot de 32 bit.
*                Bit = 1    Type caractere.
*                Bit = 0    Type different de caractere.
*                Le Status commance a gauche.
*
*         31                   0
*          --------------//-----
*         |    |   |   |    |   |
*          --------------//-----
*                   ^         ^
*                   |         |
*   Statu de dernier          Statu de 1er argument
*                    
*
************************************************************************/

#ifndef sgi
#define FALSE 0
#define TRUE  1 

#define PAQUOT -1
#define QUOT   '\\'
#define DER_CAR 127
#define MAX_CAR 125

typedef int32 * adresse; /* Les arguments sont passe par adresse en FORTRAN */

/*Adresse du nargum du pere pour transmission a recup_arg        */
int32 father_stack;

adresse  list_arg_moi[34];
int32      list_len_moi[32];
int32      nb_arg_moi;

adresse  list_arg_pere[34];
int32      list_len_pere[32];
int32      nb_arg_pere;

char *source, *destin;

/****************************************************************
*  Titre: COPY_MOT
*  Role : Fonction On_ligne.
*         Specifique au machine RISC, qui ont des probleme d'allignement
*         Donc ne peuvent copier un mot non alligne, on copie 4 octets.
*         Octet par Octet.
*****************************************************************/
#define  COPY_MOT(CONTENU_A,CONTENU_B)  \
          source = (char *) CONTENU_A;  \
          destin = (char *) CONTENU_B;  \
          destin[0] = source[0];        \
          destin[1] = source[1];        \
          destin[2] = source[2];        \
          destin[3] = source[3];  
#endif

/*----------------------------------------------------------------------
; ROUTINE INTERFACE DE RECUPERATION DE VALEUR D'ARGUMENT POUR UNE CHAINE DE
; CARACTERES.
;
; A CAUSE DE LA COMPATIBILITE CHARACTER <--> INTEGER/REAL*4/REAL*8, IL EST
; POSSIBLE D'OBTENIR UNE CHAINE DE CARACTERES ALORS QUE CE N'EST PAS
; UNE CHAINE DE CARACTERE QUI A ETE TRANSMISE S'IL SE TROUVE QU'ON
; TROUVE DES "\" AVEC DES CARACTERES IMPRIMABLES ENTRE LES "\".
; LE CONTROLE DE IMPRIM PAR FORMAT PERMET D'EVITER CELA MAIS EN MODE
; FORMAT PAR DEFAUT, CELA PEUT ARRIVER.
;
; CETTE COMPATIBILITE EST INDISPENSABLE DANS UNE PHASE TRANSITOIRE.
;
;
; DANS LE CAS D'UN NOMBRE FIXE D'ARGUMENTS, IL PEUT Y AVOIR DES ARGUMENTS
; QUI PEUVENT ETRE DES CHAINES DE CARACTERES DE TYPE CHARACTER OU NON
; DANS LE "PROGRAMME APPELANT".
; PAR SUITE DE LA COMPATIBILITE QUE L'ON DOIT ASSURER DANS UNE PHASE 
; TRANSITOIRE, LES ARGUMENTS FORMELS DE TYPE CHARACTER NE PEUVENT ETRE UTILISES ; DANS LE PROGRAMME APPELE;
; EN EFFET, SI L'ON AVAIT UN TYPE CHARACTER DANS LE "PROGRAMME APPELANT",
; ON POURRAIT MANIPULER DIRECTEMENT LA CHAINE DE CARACTERES TRANSMISE 
; MAIS SI L'ON N'AVAIT PAS UN TYPE CHARACTER (UN GEOMETRIQUE PAR EXEMPLE),
; ON NE POURRAIT ACCEDER A SA VALEUR. POUR CETTE RAISON,
; NARCAR PEUT RENDRE LA VALEUR DES QUATRE PREMIERS OCTETS (32 BITS). ON PEUT
; AINSI MANIPULER ET TESTER LA VALEUR DE L'ARGUMENT NON CHARACTER.
;
; SI ON APPELLE NARCAR AVEC UNE VARIABLE DE TYPE CHARACTER (SYNTAXE A 3/4 ARG.)
; ON COPIE AU MAXIMUM LE NOMBRE DE CARACTERES CORRESPONDANT A LA LONGUEUR DE
; CETTE VARIABLE (COMPLEMENT A BLANC OU TRONCATION EVENTUELLE)
;
; SI ON APPELLE NARCAR AVEC UN TABLEAU NON CHARACTER (SYNTAXE A 4/5 ARG.)
; ON PREND POUR LONGUEUR (EN CARACTERES) LA VALEUR FOURNIE DANS MAX ET ON
; COMPLETE A BLANC OU ON TRONQUE.
;
; LA CHAINE DE CARACTERES DU "PROGRAMME APPELANT" DOIT ETRE COMPRISE ENTRE
; BACKSLASH ET ETRE DE LONGUEUR INFERIEURE A 128 ("\" Y COMPRIS).
;
; SI L'ON N'A PAS DE "\" OU BIEN, SI L'ON N'A PAS UNE CONSTANTE/VARIABLE/
; TABLEAU DE TYPE CHARACTER DANS LE "PROGRAMME APPELANT" ET S'IL Y A DES
; CARACTERES INVALIDES ENTRE LES "\", NBR VAUDRA -1 ( <==> PAQUOT) .
;
; SI NBR > MAX , C'EST QU'IL Y A EU TRONCATION.
;
; SI ON A UNE CHAINE DE CARACTERES DE LA FORME   "\\"  , NBR SERA NUL.
; DES BLANCS SERONT MIS DANS LA CHAINE OU LE TABLEAU RECEPTEUR.
;
; NARCAR : 
; ========
; 
;
; NBR = NARCAR(LLL999,NUM,MAX,TAB[,VALARG])
;
; NUM    : NUMERO DE L'ARGUMENT DANS LE PROGRAMME APPELANT.
; MAX    : NOMBRE MAXIMUM DE CARACTERES A RECUPERER.
; TAB    : TABLEAU NON CHARACTER OU SERONT RECOPIES LES CARACTERES.
; NBR    : NOMBRE DE CARACTERES DANS LE "PROGRAMME APPELANT".
; VALARG : VALEUR DE L'ARGUMENT (PREMIERS 32 BITS). OPTIONNEL
;
;
; NBR = NARCAR(LLL999,NUM,TAB[,VALARG])
;
; NUM    : NUMERO DE L'ARGUMENT DANS LE PROGRAMME APPELANT
; TAB    : VARIABLE DE TYPE CHARACTER OU SERONT RECOPIES LES CARACTERES
; NBR    : NOMBRE DE CARACTERES DANS LE "PROGRAMME APPELANT".
; VALARG : VALEUR DE L'ARGUMENT (PREMIERS 32 BITS). OPTIONNEL
;
; IL PEUT Y AVOIR AMBIGUITE ENTRE LES DEUX SYNTAXES SUIVANTES :
;
; NARCAR(LLL999,NUM,MAX,TAB) ET NARCAR(LLL999,NUM,TAB,VALARG)
;
; S'IL Y A UNE TABLE DES LONGUEURS DANS LE PROGRAMME QUI APPELLE NARCAR, C'EST
; LA PREMIERE SYNTAXE.
; S'IL N'Y A PAS DE TABLE DES LONGUEURS DANS LE PROGRAMME QUI APPELLE NARCAR,
; C'EST LA SECONDE SYNTAXE.
;------------------------------------------------------------------------*/
#ifndef sgi
int32
narcar_ (int32 *stp)

#else
int32
narcar_(int32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
#endif

{
    char *tab,*arg_pere;     
    int32  num,max,nbr;
    int32 i;

/*=-=-=-=-=  Cette partie est strictement UNIX  =-=-=-=-=-=-=-=-=-=-==-=-=-=-=*/
#ifndef sgi
    father_stack = *stp;
    res = recup_arg_();/* La fonction qui copie les argument dans le tableau */
                       /* "list_arg_moi", et position "nb_arg_moi"  =-=-=-=-=*/
    if (res == 0) {
       printf(" %%VARARG-ERR :  FATAL ERROR \n");
       exit (-1);
    } else if (res == 2) return (0);

#else
   set_args((uint32 *) father_frame, (uint32 *) comargs__);
#endif
/*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
    
    num = *(list_arg_moi[1]);  /* Numero d'argument pere */
    if ((num > nb_arg_pere) || (num <= 0)) {
           return(PAQUOT);  /* Erreur */
    }

    arg_pere = (char *)list_arg_pere[num-1];
 
    switch (nb_arg_moi) {

          case 5 : /*=-= Syntaxe NARCAR(LLL999,NUM,MAX,TAB,VALARG)  =-=*/
                   max = *(list_arg_moi[2]);
                   tab = (char *)list_arg_moi[3];
                   COPY_MOT(arg_pere, list_arg_moi[4]);
                   break;

          case 3 : /*=-= Syntaxe NARCAR(LLL999,NUM,TAB)             =-=*/
                   max = list_len_moi[2];  /* Max = lengueur de chaine */
                   tab = (char *)list_arg_moi[2];
                   break;

          case 4 : /*=-= Syntaxe ambigue =-=-=-=-=-=-=*/
                   if (list_len_moi[2] > 0){  /* 3eme argument de type cara */
                      /*=-= Syntaxe NARCAR(LLL999,NUM,TAB,VALARG) =-=*/
                      max = list_len_moi[2]; /* Max = lengueur de chaine */
                      tab = (char *)list_arg_moi[2];
                      COPY_MOT(arg_pere, list_arg_moi[3]);
                    }else { /* 3eme argument de type autre que car */
                      /*=-= Syntaxe NARCAR(LLL999,NUM,MAX,TAB)  =-=*/
                      max = *(list_arg_moi[2]);
                      tab = (char *)list_arg_moi[3];
                   } 
                   break;
          default : /*=-= Syntaxe incorrect =-=-=*/
                   return(PAQUOT);  /* Erreur */
    }

    if (*(arg_pere++) != QUOT) {
           return(PAQUOT);  /* Erreur il faut que la chaine commance par QUOT */
    }
    
    /*=-= On commance a copier les caracteres de l'appelant vers l'appele =-=*/
    for (nbr=0; arg_pere[nbr] != QUOT && nbr<MAX_CAR; nbr++) ;

    if (arg_pere[nbr] != QUOT) {
        return(PAQUOT);  /* Erreur il faut que la chaine termine par QUOT */
    }
   
    /*=-=-= Tout est bon on copie les caractere =-=-=-=-=*/
    for (i=0; i < nbr; i++) {
         if (i < max) {
             *(tab++) = *(arg_pere++);
         }
    } 

    /*=-= On rempil a blancs l'argument de l'appele =-=-=*/
    for (i=nbr; i < max; i++) {
         *(tab++) = ' ';
    }

    return(nbr);
}

/*------------------------------------------------------------------------
;
; NTABCA : 
; ========
;
; NBR = NTABCA(LLL999,NUM,MAX,TAB[,VALARG])
;
; COMME NARCAR AVEC TAB DE TYPE CHARACTER (TABLEAU).
;
; DANS CE CAS, ON OBTIENT MAX CARACTERES DANS LE TABLEAU CHARACTER TAB QUI
; NATURELLEMENT DOIT ETRE CORRECTEMENT DIMENSIONNE. (IL N'Y A PAS DE
; CONTROLE SUR LA DIMENSION DE TAB DANS NTABCA).
;
;
; IBM ( a titre indicatif )
; =====
;
; COMME SUR VAX AVEC LES DIFFERENCES SUIVANTES :
;
; S'IL N'Y A PAS DE LONGUEUR DANS LE "PROGRAMME APPELANT" ET DONC PAS DE 
; VARIABLE/TABLEAU/CONSTANTE CHARACTER DANS LA LISTE D'ARGUMENTS, ON RECHERCHE
; LES "#" QUI CORRESPONDENT AUX "\" DU VAX ET ON CONTROLE QUE L'ON A DES
; CARACTERES IMPRIMABLES.
;
; S'IL Y A UNE LONGUEUR DIFFERENTE DE 4 ET DE 8 DANS LE "PROGRAMME APPELANT",
; C'EST UNE VARIABLE/TABLEAU/CONSTANTE CHARACTER ET L'ON TIENT COMPTE DE CETTE
; LONGUEUR.
;
; S'IL Y A UNE LONGUEUR 4 OU 8 DANS LE "PROGRAMME APPELANT", IL PEUT Y AVOIR
; AMBIGUITE. S'IL N'Y A QU'UN ARGUMENT DANS LE "PROGRAMME APPELANT", IL N'Y A
; PAS D'AMBIGUITE ET CET ARGUMENT EST DE TYPE CHARACTER DE LONGUEUR 4 OU 8.
; S'IL Y A PLUS D'UN ARGUMENT DANS LE "PROGRAMME APPELANT", ON CONTROLE COMME
; S'IL N'Y AVAIT PAS DE LONGUEUR.
;---------------------------------------------------------------------------*/

#ifndef sgi
int32
ntabca_(int *stp)
#else
int32
ntabca_(int32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
#endif

{
    char *tab,*arg_pere;     
    int32  num,max,nbr;
    int32 i;

/*=-=-=-=-=  Cette partie est strictement UNIX  =-=-=-=-=-=-=-=-=-=-==-=-=-=-=*/
#ifndef sgi
    father_stack = *stp;
    res = recup_arg_();/* La fonction qui copie les argument dans le tableau */                        /* "list_arg_moi", et position "nb_arg_moi"  =-=-=-=-=*/
/*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
    
    if (res == 0) {
       printf(" %%VARARG-ERR :  FATAL ERROR \n");
       exit (-1);
    } else if (res == 2) return (0);
#else
   set_args((uint32 *) father_frame, (uint32 *) comargs__);
#endif

    if (nb_arg_moi < 4 || nb_arg_moi > 5) {
             return(PAQUOT); /* Erreur de Syntaxe */
    }

    num = *(list_arg_moi[1]);  /* Numero d'argument pere */
    if ((num > nb_arg_pere) || (num <= 0)) {
           return(PAQUOT);  /* Erreur */
    }

    arg_pere = (char *)list_arg_pere[num-1];
    max = *(list_arg_moi[2]);
 
    if (nb_arg_moi == 5) {/* Syntaxe NTABCA(LLL999,NUM,MAX,TAB,VALARG) */
            /*=-= On copie dans VAlARG, le 1er mot de l'arg_pere =-=-=*/
            COPY_MOT(arg_pere, list_arg_moi[4]);
    }

    if (*(arg_pere++) != QUOT) {
           return(PAQUOT);  /* Erreur il faut que la chaine commance par QUOT */
    }
    
    for (nbr=0; *(arg_pere) != QUOT && nbr<MAX_CAR; nbr++, arg_pere++) {
         if (nbr < max) {
             *(tab++) = *(arg_pere);
         }
    } 

    if (*(arg_pere) != QUOT) {
        return(PAQUOT);  /* Erreur il faut que la chaine termine par QUOT */
    }

    for (i=nbr; i < max; i++) {
         *(tab++) = ' ';
    }

    return(nbr);
}

/*---------------------------------------------------------------------------
; ========
; NCHARA :
; ========
;
; DANS CE CAS, ON EST A ARGUMENTS EN NOMBRE FIXE : NOMS EUCLID, PROGRAMMES DE
; BD, ...
; IL N'Y A ALORS PAS DE "\" ET L'ON DOIT SEULEMENT ASSURER LA COMPATIBILITE
; AVEC LE FONCTIONNEMENT ACTUEL (FORTRAN 66).
;
; VAX :
; =====
;
; ON RETROUVE LES MEME SYNTAXES QUE POUR NARCAR :
;
; NBRCAR=NCHARA(LLL999,NUM,MAXCAR,TABCAR[,VALARG])
;
; NBRCAR=NCHARA(LLL999,NUM,TABCAR[,VALARG])
;
; SI L'ON N'A PAS UN TYPE CHARACTER DANS LE "PROGRAMME APPELANT", ON FOURNIT
; LES PREMIERS CARACTERES VALIDES (IMPRIMABLES) ET ON COMPLETE EVENTUELLEMENT
; A BLANC JUSQU'A MAXCAR CARACTERES ET NBRCAR VAUT MOINS LE NOMBRE DE
; CARACTERES VALIDES TROUVES. VALARG CONTIENT ALORS LE PREMIER MOT (32 BITS)
; ET LE FONCTIONNEMENT EST SEMBLABLE AU FONCTIONNEMENT ACTUEL.
;
; SI L'ON A UN TYPE CHARACTER DANS LE "PROGRAMME APPELANT", ON LES FOURNIT
; DANS TABCAR ET NBRCAR VAUT LE NOMBRE DE CARACTERES QU'IL Y AVAIT DANS LE
; "PROGRAMME APPELANT".
;---------------------------------------------------------------------------*/
#ifndef sgi
int32
nchara_(int32 *stp)
#else
int32
nchara_(int32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
#endif

{
    char *tab,*arg_pere;     
    int32  num,max,nbr,max_car_pere;
    int32 i;

/*=-=-=-=-=  Cette partie est strictement UNIX  =-=-=-=-=-=-=-=-=-=-==-=-=-=-=*/
#ifndef sgi
    father_stack = *stp;
    res = recup_arg_();/* La fonction qui copie les argument dans le tableau */                        /* "list_arg_moi", et position "nb_arg_moi"  =-=-=-=-=*/
/*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
    
    if (res == 0) {
       printf(" %%VARARG-ERR :  FATAL ERROR \n");
       exit (-1);
    } else if (res == 2) return (0);
#else
   set_args((uint32 *) father_frame, (uint32 *) comargs__);
#endif

    num = *(list_arg_moi[1]);  /* Numero d'argument pere */
    if ((num > nb_arg_pere) || (num <= 0)) {
           return(FALSE);  /* Erreur */
    }

    arg_pere = (char *)list_arg_pere[num-1];
 
    switch (nb_arg_moi) {

          case 5 : /*=-= Syntaxe NARCAR(LLL999,NUM,MAX,TAB,VALARG)  =-=*/
                   max = *(list_arg_moi[2]);
                   tab = (char *)list_arg_moi[3];
                   COPY_MOT(arg_pere, list_arg_moi[4]);
                   break;

          case 3 : /*=-= Syntaxe NARCAR(LLL999,NUM,TAB)             =-=*/
                   max = list_len_moi[2];  /* Max = lengueur de chaine */
                   tab = (char *)list_arg_moi[2];
                   break;

          case 4 : /*=-= Syntaxe ambigue =-=-=-=-=-=-=*/
                   if (list_len_moi[2] > 0){  /* 3eme argument de type cara */
                      /*=-= Syntaxe NARCAR(LLL999,NUM,TAB,VALARG) =-=*/
                      max = list_len_moi[2]; /* Max = lengueur de chaine */
                      tab = (char *)list_arg_moi[2];
                      COPY_MOT(arg_pere, list_arg_moi[3]);
                    }else { /* 3eme argument de type autre que car */
                      /*=-= Syntaxe NARCAR(LLL999,NUM,MAX,TAB)  =-=*/
                      max = *(list_arg_moi[2]);
                      tab = (char *)list_arg_moi[3];
                   } 
                   break;
          default : /*=-= Syntaxe incorrect =-=-=*/
                   return(FALSE);  /* Erreur */
    }

    max_car_pere = list_len_pere[num - 1];

    if (max_car_pere == 0) { /* Argument pere de type autre que car */
       /* On copie tous les caractere valide */
       for(nbr=0;nbr<max &&((*arg_pere>=' ')&&(*arg_pere<DER_CAR));nbr++) {
             *(tab++) = *(arg_pere++);
       }
     }else {/* Argument pere de type car */
       /* On copie tous les caractere */
       for(nbr=0; nbr<max  && nbr<max_car_pere;nbr++) {
             *(tab++) = *(arg_pere++);
       }
    }

    /*=-= On rempil a blancs l'argument de l'appele =-=-=*/
    for (i=nbr; i < max; i++) {
         *(tab++) = ' ';
    }

    if (max_car_pere == 0) {
             return(-nbr);    
     }else {
             return(max_car_pere);
    }
}

/*-------------------------------------------------------------------------
; NTABCH :
; ========
;
; NBR = NTABCH(LLL999,NUM,MAX,TAB[,VALARG])
;
; COMME NCHARA AVEC TAB DE TYPE CHARACTER (TABLEAU).
;
; DANS CE CAS, ON OBTIENT MAX CARACTERES DANS LE TABLEAU CHARACTER TAB QUI
; NATURELLEMENT DOIT ETRE CORRECTEMENT DIMENSIONNE. (IL N'Y A PAS DE
; CONTROLE SUR LA DIMENSION DE TAB DANS NTABCH).
;
; IBM ( a titre indicatif )
; =====
;
; A PARTIR DU MOMENT OU ON VEUT ASSURER LA COMPATIBILITE CHARACTER ET
; ANCIEN MODE D'APPEL AVEC INTEGER/REAL*4/REAL*8, IL Y A DES AMBIGUITES.
; CEPENDANT IL FAUT SAVOIR QUE CELA N'AJOUTE PAS D'AMBIGUITES PAR RAPPORT
; A L'ETAT ACTUEL BIEN AU CONTRAIRE.
;
; S'IL N'Y A PAS DE TABLE DES LONGUEURS DANS LE "PROGRAMME APPELANT", ON
; FOURNIT MAXCAR CARACTERES (LES PREMIERS CARACTERES IMPRIMABLES SUIVIS DE
; BLANCS) ET NBRCAR=0.
;
; S'IL Y A UNE VARIABLE/TABLEAU/CONSTANTE CHARACTER DANS LA LISTE, CE N'EST
; PAS FORCEMENT CELUI QUE L'ON CHERCHE A RECUPERER PAR NCHARA.
;
; SI LA LONGUEUR EST 4, ON FOURNIT MAXCAR CARACTERES ET NBRCAR VAUT 0 A MOINS
; QUE L'ON N'AIT QU'UN SEUL ARGUMENT DANS LE "PROGRAMME APPELANT" AUQUEL CAS
; CET ARGUMENT EST DE TYPE CHARACTER DE LONGUEUR 4 OU 8. S'IL Y A PLUS D'UN
; ARGUMENT, ON NE PEUT SAVOIR SI 4 CARACTERES ONT ETE TRANSMIS (TYPE
; CHARACTER*4) OU BIEN SI C'EST UNE VARIABLE ENTIERE/REELLE OU BIEN SI C'EST UN
; TABLEAU ENTIER/REEL.
;
; SI LA LONGUEUR EST DIFFERENTE DE 4, ON FOURNIT LES CARACTERES ET NBRCAR
; VAUT LE NOMBRE DE CARACTERES.
;----------------------------------------------------------------------------*/
#ifndef sgi
int32
ntabch_(int32 *stp)

#else
int32
ntabch_(int32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
#endif

{
    char *tab,*arg_pere;     
    int32  num,max,nbr,max_car_pere;
    int32 i;

/*=-=-=-=-=  Cette partie est strictement UNIX  =-=-=-=-=-=-=-=-=-=-==-=-=-=-=*/
#ifndef sgi
    father_stack = *stp;
    res = recup_arg_();/* La fonction qui copie les argument dans le tableau */                        /* "list_arg_moi", et position "nb_arg_moi"  =-=-=-=-=*/ /*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
    
    if (res == 0) {
       printf(" %%VARARG-ERR :  FATAL ERROR \n");
       exit (-1);
    } else if (res == 2) return (0);
#else
   set_args((uint32 *) father_frame, (uint32 *) comargs__);
#endif

    if (nb_arg_moi < 4 || nb_arg_moi > 5) {
             return(FALSE); /* Erreur de Syntaxe */
    }

    num = *(list_arg_moi[1]);  /* Numero d'argument pere */
    if ((num > nb_arg_pere) || (num <= 0)) {
           return(FALSE);  /* Erreur */
    }

    arg_pere = (char *)list_arg_pere[num-1];
    max = *(list_arg_moi[2]);
 
    if (nb_arg_moi == 5) {/* Syntaxe NTABCA(LLL999,NUM,MAX,TAB,VALARG) */
            /*=-= On copie dans VAlARG, le 1er mot de l'arg_pere =-=-=*/
            COPY_MOT(arg_pere, list_arg_moi[4]);
    }
    max_car_pere = list_len_pere[num - 1];

    if (max_car_pere == 0) { /* Argument pere de type autre que car */
       /* On copie tous les caractere valide */
       for(nbr=0;nbr<max &&((*arg_pere>=' ')&&(*arg_pere<DER_CAR));nbr++) {
             *(tab++) = *(arg_pere++);
       }
     }else {/* Argument pere de type car */
       /* On copie tous les caractere */
       for(nbr=0; nbr<max  && nbr<max_car_pere;nbr++) {
             *(tab++) = *(arg_pere++);
       }
    }

    /*=-= On rempil a blancs l'argument de l'appele =-=-=*/
    for (i=nbr; i < max; i++) {
         *(tab++) = ' ';
    }

    if (max_car_pere == 0) {
             return(-nbr);    
     }else {
             return(max_car_pere);
    }
}

/*------------------------------------------------------------------------
; ========
; NRETCA :
; ========
;
; SYNTAXE :
;       NBR = NRETCA(LLL999,NUM,TAB)
;       TAB : LA CHAINE A RENVOYER, DE TYPE CARACTERE.
;
; SYNTAXE SUPPLEMENTAIRE :
;       NBR = NRETCA(LLL999,NUM,MAX,TAB)
;
; NRETCA PERMET DE RENVOYER DANS LE PROGRAMME APPELANT LA VALEUR D'UNE CHAINE
; DE CARACTERES AVEC LES MEMES SYNTAXES QUE CI-DESSUS SANS L'ARGUMENT OPTIONNEL
; VALARG.
;
; LA VALEUR DE LA FONCTION NRETCA EST LE NOMBRE DE CARACTERES RECOPIES DANS LE
; PROGRAMME APPELANT. CE NOMBRE EST :
;                     > 0 SI ON A UNE VARIABLE CHARACTER DANS L'APPELANT
;                     < 0 SI ON N'A PAS UNE VARIABLE CHARACTER DANS L'APPELANT
;
; CE NOMBRE PEUT ETRE SUPERIEUR AU NOMBRE DE CARACTERES TRANSMIS PAR LE
; PROGRAMME QUI APPELLE NRETCA SI ON A UNE VARIABLE CHARACTER DANS LE
; "PROGRAMME APPELANT" CAR ON COMPLETE A BLANC. S'IL EST INFERIEUR, C'EST
; QU'IL Y A EU TRONCATION.
;
; SI L'ARGUMENT DU "PROGRAMME APPELANT" N'EST PAS DE TYPE CHARACTER, IL N'Y
; A PAS DE COMPLEMENT A BLANC ET ON NE TRANSFERE QUE LE NOMBRE DE CARACTERES
; TRANSMIS A NRETCA.
;
; SYNTAXE SUPPLEMENTAIRE :
;
; NBR = NRETCA(LLL999,NUM,MAX,TAB)
;
; CETTE SYNTAXE EST DESTINEE A RENVOYER UN TABLEAU DE TYPE CHARACTER ET
; TAB EST DE TYPE CHARACTER. (C'EST L'EQUIVALENT DE NTABCA ET DE NTABCH).
;-----------------------------------------------------------------------*/
#ifndef sgi
int32
nretca_(int32 *stp)

#else
int32
nretca_(int32 *father_frame, int32 dum1, int32 dum2, int32 dum3)
#endif

{
    char *tab,*arg_pere;     
    int32  num,max,nbr,max_car_pere;
    int32 i;

/*=-=-=-=-=  Cette partie est strictement UNIX  =-=-=-=-=-=-=-=-=-=-==-=-=-=-=*/
#ifndef sgi
    father_stack = *stp;
    res = recup_arg_();/* La fonction qui copie les argument dans le tableau */
                       /* "list_arg_moi", et position "nb_arg_moi"  =-=-=-=-=*/ /*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/
    
    if (res == 0) {
       printf(" %%VARARG-ERR :  FATAL ERROR \n");
       exit (-1);
    } else if (res == 2) return (0);
#else
   set_args((uint32 *) father_frame, (uint32 *) comargs__);
#endif

    num = *(list_arg_moi[1]);  /* Numero d'argument pere */
    if ((num > nb_arg_pere) || (num <= 0)) {
           return(FALSE);  /* Erreur */
    }

    arg_pere = (char *)list_arg_pere[num-1];
 
    switch (nb_arg_moi) {

          case 4 : /*=-= Syntaxe NRETCA(LLL999,NUM,MAX,TAB)  =-=*/
                   max = *(list_arg_moi[2]);
                   tab = (char *)list_arg_moi[3];
                   break;

          case 3 : /*=-= Syntaxe NARCAR(LLL999,NUM,TAB)             =-=*/
                   max = list_len_moi[2];  /* Max = lengueur de chaine */
                   tab = (char *)list_arg_moi[2];
                   break;

          default : /*=-= Syntaxe incorrect =-=-=*/
                   return(FALSE);  /* Erreur */
    }

    max_car_pere = list_len_pere[num - 1];

    if (max_car_pere == 0) { /* Argument pere de type autre que car */
       /* On copie tous les caractere */
       for(nbr=0; nbr<max; nbr++) {
             *(arg_pere++) = *(tab++);
       }
     }else {/* Argument pere de type car */
       /* On copie tous les caractere */
       for(nbr=0; nbr<max  && nbr<max_car_pere;nbr++) {
             *(arg_pere++) = *(tab++);
       }
       /*=-= On rempil a blancs l'argument de l'appele =-=-=*/
       for (i=nbr; i < max_car_pere; i++) {
         *(arg_pere++) = ' ';
       }
    }

    if (max_car_pere == 0) {
             return(-nbr);    
     }else {
             return(max_car_pere);
    }
}
