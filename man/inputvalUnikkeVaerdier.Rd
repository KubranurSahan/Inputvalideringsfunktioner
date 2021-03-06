% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InputValideringsfunktioner.R
\encoding{UTF-8}
\name{inputvalUnikkeVaerdier}
\alias{inputvalUnikkeVaerdier}
\title{Inputvalidering (Unikke værdier)}
\usage{
inputvalUnikkeVaerdier(
  datasaet,
  datasaetUnikkeVariableListe,
  sti = NA,
  datasaetNavn = NA,
  meldFejl = TRUE
)
}
\arguments{
\item{datasaet}{en data.frame, Datasættet som skal gennemses}

\item{datasaetUnikkeVariableListe}{en character vektor, En liste over de variable der skal være unikke}

\item{sti}{en chracter string, Stien til den rapport, som outputtet af funktionen skal sendes til. Rapporten skal være en txt-fil.}

\item{datasaetNavn}{en chracter string, Brugerdefineret navn til at definere datasættet i rapporten. Hvis NA (default) bruges objektnavnet på datasættet fra miljøet.}

\item{meldFejl}{TRUE/FALSE værdi, Hvis TRUE melder fejl, hvis de variable der skal være unikke ikke indeholder unikke-værdier}
}
\value{
Tekst med information om hvorvidt de variable i datasættet som skal være unikker er unikke.
}
\description{
Funktionen bruges til at se om en liste af variable der hver især skal indeholde unikke værdier gennem hele datasættet,
    indeholder unikke værdier. Er dette ikke tilfældet meldes der fejl som default.
    Funktionenens resultater skal ikke nødvendigvis skrives til en rapport, men det er muligt at gøre dette ved at definere stien.
}
