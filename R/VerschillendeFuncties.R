#' normaldichtheidsfunctie
#'
#' Functie tekent de dichtheidsfunctie van een normale verdeling.
#'
#' @param gemiddelde gemiddelde van de normale verdeling (default = 0)
#' @param standaardafwijking standaardafwijking van de normale verdeling (default = 1)
#'
#' @return grafiek van de dichtheidsfunctie
#' @export
#'
#' @examples normaldichtheidsfunctie(0,1)

normaldichtheidsfunctie=function(gemiddelde=0,standaardafwijking=1)
{
  xlijst=seq(gemiddelde-4*standaardafwijking,gemiddelde+4*standaardafwijking,by=0.01)
  ylijst=dnorm(xlijst,mean=gemiddelde,sd=standaardafwijking)
  plot(xlijst,ylijst,type="l",main=paste("dichtheidsfunctie, mu =",gemiddelde,"en sigma =",standaardafwijking),xlab="",ylab="")
}

#' kansnormaleverdeling
#'
#' Functie bepaalt bij een normale verdeling de kans en tekent de bijhorende oppervlakte.
#'
#' @param beginwaarde beginwaarde van het interval (default = -oneindig)
#' @param eindwaarde eindwaarde van het interval (default = + oneindig)
#' @param gemiddelde gemiddelde van de normale verdeling (default = 0)
#' @param standaardafwijking standaardafwijking van de normale verdeling (default = 1)
#'
#' @return waarde van de kans en tekening van oppervlakte onder dichtheidsfunctie
#' @export
#'
#' @examples kansnormaleverdeling(beginwaarde=2)

kansnormaleverdeling=function(beginwaarde=-Inf, eindwaarde=+Inf, gemiddelde=0, standaardafwijking=1)
{
  xlijst=seq(gemiddelde-4*standaardafwijking,gemiddelde+4*standaardafwijking,by=0.01)
  ylijst=dnorm(xlijst,mean=gemiddelde,sd=standaardafwijking)
  plot(xlijst,ylijst,type="l",main="",xlab="",ylab="",axes=F)
  if(beginwaarde==-Inf & eindwaarde==+Inf){
    kans=1
    polygon(c(xlijst,gemiddelde+4*standaardafwijking,gemiddelde-4*standaardafwijking),c(ylijst,0,0),col='blue')
  }
  else{
    if(beginwaarde==-Inf&eindwaarde<+Inf)
    {
      kans=pnorm(eindwaarde, mean=gemiddelde, sd=standaardafwijking)
      xrange=seq(gemiddelde-4*standaardafwijking,eindwaarde,length.out=100)
      polygon(c(xrange,eindwaarde,gemiddelde-4*standaardafwijking),c(dnorm(xrange,mean=gemiddelde,sd=standaardafwijking),0,0),col='blue')
    }
    else{
      if(beginwaarde>-Inf & eindwaarde==+Inf)
      {
        kans=pnorm(beginwaarde,mean=gemiddelde,sd=standaardafwijking,lower.tail=FALSE)
        xrange=seq(beginwaarde,gemiddelde+4*standaardafwijking,length.out=100)
        polygon(c(xrange,gemiddelde+4*standaardafwijking,beginwaarde),c(dnorm(xrange,mean=gemiddelde,sd=standaardafwijking),0,0),col='blue')
      }
      else {
        kans=pnorm(eindwaarde,mean=gemiddelde,sd=standaardafwijking)-pnorm(beginwaarde,mean=gemiddelde,sd=standaardafwijking)
        xrange=seq(beginwaarde,eindwaarde,length.out=100)
        polygon(c(xrange,eindwaarde,beginwaarde),c(dnorm(xrange,mean=gemiddelde,sd=standaardafwijking),0,0),col='blue')
      }}}
  axis(side=1,at=gemiddelde+(-4:4)*standaardafwijking,pos=0)
  kans
}

#' kansbinomiale
#'
#' Functie bepaalt bij een binomiale verdeling de kans en tekent de bijhorende oppervlakte.
#'
#' @param kans waarde van parameter p (0<p<1) horend bij de binomiale verdeling
#' @param aantal waarde van de parameter n (natuurlijk getal) horend bij de normale verdeling
#' @param beginwaarde beginwaarde van het interval (default = 0)
#' @param eindwaarde eindwaarde van het interval (default = NULL)
#'
#' @return waarde van de kans en tekening van de oppervlakte onder de grafiek
#' @export
#'
#' @examples kansbinomiale(0.4,10,3,5)

kansbinomiale=function(kans,aantal,beginwaarde=0,eindwaarde=NULL)
{
  if((kans<=0) | (kans>=1)){stop("Zorg dat 0<p<1")}
  if((aantal<0)|!(aantal==round(aantal))){stop("aantal moet een natuurlijk getal zijn")}
  if(is.null(eindwaarde)){eindwaarde=aantal}
  if(eindwaarde>aantal){
    stop("eindwaarde is een natuurlijk getal tussen 0 en n")}
  probs=dbinom(0:aantal,size=aantal, prob=kans)
  if((beginwaarde==0)&(eindwaarde==aantal))
  {
    kleur="blue"
    uitkomstkans=1}
  if((beginwaarde==0)&(eindwaarde<aantal))
  {
    kleur=c(rep("blue",eindwaarde+1),rep("grey",aantal-eindwaarde))
    uitkomstkans=pbinom(eindwaarde,aantal,kans)
  }
  if((beginwaarde>0)&(eindwaarde==aantal))
  {
    kleur=c(rep("grey",beginwaarde),rep("blue",aantal+1-beginwaarde))
    uitkomstkans=1-pbinom(beginwaarde-1,aantal,kans)
  }
  if((beginwaarde>0)&(eindwaarde<aantal))
  {
    kleur=c(rep("grey",beginwaarde),rep("blue",eindwaarde-beginwaarde+1), rep("grey",aantal-eindwaarde))
    uitkomstkans=pbinom(eindwaarde,aantal,kans)-pbinom(beginwaarde-1,aantal,kans)
  }
  barplot(probs,names.arg=c(0:aantal),col=kleur)
  uitkomstkans
}

#' cirkeldiagram
#'
#' Functie maakt een cirkeldiagram van een categorische variabele, waarbij de categorieën in een legende verschijnen en de relatieve frequenties bij het cirkeldiagram zelf.
#'
#' @param categorischevariabele de variabele
#' @param ... eventuele andere parameters
#'
#' @return cirkeldiagram
#' @export
#'
#' @examples cirkeldiagram(c('cat-a','cat-a','cat-b','cat-c','cat-a','cat-b'))

cirkeldiagram=function(categorischevariabele,...)
{
  Freq=table(categorischevariabele)
  kleurtjes=rainbow(length(Freq))
  Eigenlabels=prop.table(Freq)*100
  Eigenlabels=round(Eigenlabels,2)
  Eigenlabels=paste(Eigenlabels,"%",sep=" ")
  pie(Freq,labels=Eigenlabels,col=kleurtjes,...)
  legend("topleft",names(Freq),fill=kleurtjes)
}

#' Modus
#'
#' Functie bepaalt de centrummaat modus van een dataset.
#' Indien er verschillende waardes zijn die het meest voorkomen, dan worden deze allemaal bepaald.
#'
#' @param x een dataset
#'
#' @return de modus of modi van x
#' @export
#'
#' @examples modus(c(1,1,2,2,3))
#' modus(c('a','u','t','o','b','u','s'))

modus=function(x)
{
  u=unique(x)
  tab=tabulate(match(x, u))
  u[tab == max(tab)]
}
