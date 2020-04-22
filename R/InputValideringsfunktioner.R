### Funktion til at gennemse datasættet for mangler og værdisæt for hver variabel med udgangspunkt i den tilhørende formatfil ###
# Funktionen består af to dele:
# 1) Første del gennemser datasættet for mangler og udskriver en tabel med andelen af mangler og bemærkninger.
#    Bemærkninger sættes kun på de variable, hvor der mangler mere end tilladt.
#    Funktionen melder herefter fejl i tilfælde af at der mangler mere end tilladt i mindst en variabel, hvis dette ønskes.
# 2) Anden del tester værdier i datasættets variable mod den tilhørende værdiliste (defineret i formatfilen).
#    Der udskrives en tabel for hver enkelt variabel med de værdier som ikke er tilladt/accepteret, hvis der findes sådanne værdier.
#    Funktionen melder herefter fejl i tilfælde af at der findes mindst en variabel med ikke tilladte værdier, hvis dette ønskes

#' Inputvalidering (Mangler og værdier)
#' @encoding UTF-8
#'
#' @description Funktion til at gennemse datasættet for mangler og værdisæt for hver variabel med udgangspunkt i den tilhørende formatfil.
#'     Funktionen består af to dele:\cr
#'     \itemize{
#'     \item Første del gennemser datasættet for mangler og udskriver en tabel med andelen af mangler og bemærkninger.\cr
#'     Bemærkninger sættes kun på de variable, hvor der mangler mere end tilladt.\cr
#'     Funktionen melder herefter fejl i tilfælde af, at der mangler mere end tilladt i mindst en variabel, hvis dette ønskes.
#'     \item Anden del tester værdierne i datasættets variable mod den tilhørende værdiliste (defineret i formatfilen).\cr
#'     Der udskrives en tabel for hver enkelt variabel med de værdier som ikke er tilladt/accepteret, hvis der findes sådanne værdier.\cr
#'     Funktionen melder herefter fejl i tilfælde af at der findes mindst en variabel med ikke tilladte værdier, hvis dette ønskes.
#'     }
#'
#' @param datasaet en data.frame, Datasættet som skal gennemses for mangler og værdier
#' @param datasaetTilladtMangel En data.frame med to kolonner, hvor første kolonne svarer til variabelnavnene og anden kolonne svarer til den maksimale andel der er tilladt at mangle i den enkelte variabel.
#'     Rækkefølgen af kolonnerne har betydning for om funktionen virker.
#' @param datasaetVaerdiListe En data.frame med to kolonner, hvor første kolonne svarer til variabelnavnene og anden kolonne svarer til navnet på den liste over tilladte/acceptable værdier for variablen.
#'     Rækkefølgen af kolonnerne har betydning for om funktionen virker.
#' @param sti en chracter string, Stien til den rapport, som outputtet af funktionen skal sendes til. Rapporten skal være en txt-fil hvis udskrivTekstTilRapport er TRUE ellers skal det være stien til mappen som tabellerne skal udskrives til.
#' @param udskrivTekstTilRapport TRUE/FALSE værdi, Hvis TRUE så udskrives teksterne til txt-filen som er angivet i "sti". HVis FALSE bør sti være en mappesti.
#' @param datasaetNavn en chracter string, Brugerdefineret navn til at definere datasættet i rapporten. Hvis NA (default) bruges objektnavnet på datasættet fra miljøet.
#' @param meldFejlVedMangler TRUE/FALSE værdi, Hvis TRUE melder funktionen fejl, hvis der mangler mere end tilladt i mindst en variabel i datasættet. Fejlmeldingen sker efter tabellen er udskrevet.
#' @param meldFejlVedIkkeTilladteVaerdier TRUE/FALSE værdi, Hvis TRUE melder funktionen fejl, hvis der findes ikke tilladte værdier i mindst en variabel i datasættet. Fejlmeldingen sker efter tabellen er udskrevet.
#'
#' @details Funktionen udskriver tekst til rapporten i stien, hvis udskrivTekstTilRapport er valgt.
#'     De to pdf filer, der genereres af funktionen, bliver gemt under samme manppe som rapporten eller mappen som er angivet i "sti", hvis udskrivTekstTilRapport ikke er valgt.
#'
#' @return To sæt af pdf filer, hvor den ene viser en liste over variable og hvor meget der mangler i hver variabel og den anden viser de værdier der ikke findes i værdilisten for variabelen (hvis der findes sådanne værdier).
#'
#' @export
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' @importFrom reshape2 melt
#' @importFrom stats aggregate
#' @importFrom TabellerOgGrafer udskrivTabelTilPdf
#' @importFrom diverseFunktioner procent
#'
inputvalManglerOgVaerdier <- function(datasaet,# Datasættet som skal gennemses for mangler og værdier
                                      datasaetTilladtMangel,# Et datasæt med to kolonner, hvor første kolonne svarer til variabelNavn og anden svarer til andel der må mangle
                                      datasaetVaerdiListe,# Et datasæt med to kolonner, hvor første kolonne svarer til variabelNavn og anden svarer til navnet på den liste over tilladte/acceptable værdier for variablen
                                      sti,# En sti til den rapport hvor output skal sendes til eller stien til mappen som tabellerne skal udskrives til.
                                      udskrivTekstTilRapport = TRUE,# Hvis TRUE så udskrives teksterne til txt-filen som er angivet i "sti".
                                      datasaetNavn = NA,# Brugerdefineret navn til at definere datasættet - default er at navnet på datasættet i miljøet bruges
                                      meldFejlVedMangler = FALSE,# Hvis TRUE melder fejl hvis der mangler mere end tilladt i mindst en variabel i datasættet efter tabellen er udskrevet
                                      meldFejlVedIkkeTilladteVaerdier = FALSE# Hvis TRUE melder fejl hvis der findes ikke tilladte værdier i mindst en variabel i datasættet efter tabellerne er udskrevet
){

  # Kigger på om sti er en ".txt" når "udskrivTekstTilRapport" er TRUE
  if(udskrivTekstTilRapport == TRUE){

    if(substring(sti, nchar(sti) - 3, nchar(sti)) != ".txt"){

      stop("sti skal være stien til en .txt fil.")

    }

    # Starter med at åbne rapporten
    sink(file = sti, append = TRUE)

    # Definerer stien til rapportmappen som den mappe rapportfilen ligger i
    rapportMappeSti <- substring(sti,
                                 1,
                                 gregexpr("/", sti, fixed = TRUE)[[1]][length(gregexpr("/", sti, fixed = TRUE)[[1]])])

  }else{

    # Hvis ikke udskrivTekstTilRapport er TRUE bør sti være stien til mappen og ikke en txt fil.
    rapportMappeSti <- sti

  }

  # Tjekker om mappen findes
  if(!dir.exists(rapportMappeSti)){

    stop("Mappen til sti findes ikke.")

  }

  # Definerer navnet på datasættet medmindre denne er angivet som input
  if(is.na(datasaetNavn)){

    datasaetNavn <- deparse(substitute(datasaet))

  }

  # Ændrer kolonne navnene på datasaetTilladtMangel og datasaetVaerdiListe så de passer med variabelnavne der bruges i funktionen
  # - Bemærk at dette vil fejle hvis rækkefølgen ikke er rigtig på de to variable
  colnames(datasaetTilladtMangel) <- c("variabelNavn", "tilladtMangel")
  colnames(datasaetVaerdiListe) <- c("variabelNavn", "vaerdiListe")


  cat("Datasæt", datasaetNavn, "indeholder", format(ncol(datasaet), big.mark = ",", decimal.mark = "."), "kolonner og", format(nrow(datasaet), big.mark = ",", decimal.mark = "."), "rækker.\n")

  ### Beregner andelen af tomme inputs/NA'er for hver variabel i datasættet
  datasaetMangler <- rownames_to_column(data.frame("antalNA" = colSums(is.na(datasaet))), "variabelNavn") %>%
    mutate(andelNA = procent(antalNA, nrow(datasaet))) %>%
    left_join(datasaetTilladtMangel, by = "variabelNavn") %>%
    mutate(bemaerk = ifelse(!is.na(tilladtMangel),
                            ifelse(as.numeric(gsub("%", "", andelNA)) > tilladtMangel, "Mangler mere end tilladt.", ""),
                            ""))

  # Definerer filen tabellen skal udskrives til
  mdlFil <- file.path(rapportMappeSti, paste0("Tabel over mangler i ", datasaetNavn, "-", format(Sys.time(), "%d%m%y_%H%M"), ".pdf"))

  # Skriver i rapporten, hvor tabellen ligger
  cat("Manglerne i", datasaetNavn, "datasættet kan ses i filen:\n", mdlFil, "\n")

  if(udskrivTekstTilRapport == TRUE){sink()}

  # Udskriver tabellen
  udskrivTabelTilPdf(datasaetMangler, mdlFil, titelTekst = paste0("Tabel over mangler i ", datasaetNavn))



  ### Gennemser værdierne for alle de variable, hvor der ønskes en test af værdier

  if(udskrivTekstTilRapport == TRUE){sink(file = sti, append = TRUE)}

  if(any(!is.na(datasaetVaerdiListe$vaerdiListe))){

    # Danner en ny liste med kun de variable, hvor værdilisten er defineret
    datasaetVaerdiListeA <- datasaetVaerdiListe %>%
      filter(!is.na(vaerdiListe))

    # Danner en kopi af datasaet
    datasaetMdl <- datasaet

    # Danner en signal til at melde fejl om at der findes variable med ikke tilladte værdier
    ikkeTilladteVaerdierFindes <- FALSE

    # Definerer filen tabellen skal udskrives til
    mdlFilx <- file.path(rapportMappeSti, paste0("Ikke tilladte værdier i ", datasaetNavn, "-", format(Sys.time(), "%d%m%y_%H%M")))

    # Tjekker nu for hver værdiliste de tilhørende variabler
    for(tjekModVaerdilisten in unique(datasaetVaerdiListeA$vaerdiListe)){

      # Danner en liste af alle de variable med samme værdiliste
      variabelListeMdl <- datasaetVaerdiListeA[datasaetVaerdiListeA$vaerdiListe == tjekModVaerdilisten,]$variabelNavn

      # Henter værdilisten - melder fejl hvis listen ikke findes i miljøet
      vaerdiListenMdlA <- get(tjekModVaerdilisten, envir = .GlobalEnv)

      # Sørger for at vaerdiListenMdl er en datasæt
      vaerdiListenMdl <- data.frame("vaerdi" = vaerdiListenMdlA, stringsAsFactors = FALSE) %>%
        mutate(vaerdiFindesIkkeIListen = 0)

      # Danner en longliste med værdier for de valgte variable fra datasættet
      meltedDatasaet  <-  datasaet %>%
        group_by() %>%
        select(variabelListeMdl) %>%
        melt(id.vars = NULL, na.rm = TRUE, value.name = "vaerdi" ) %>%
        left_join(vaerdiListenMdl, by = "vaerdi") %>%
        mutate(vaerdiFindesIkkeIListen = ifelse(is.na(vaerdiFindesIkkeIListen), 1, vaerdiFindesIkkeIListen))

      # Tjekker om der findes ikke tilladte/acceptreret værdier i variablene
      if(sum(meltedDatasaet$vaerdiFindesIkkeIListen) != 0){

        # Danner en signal til at melde fejl om at der findes variable med ikke tilladte værdier
        ikkeTilladteVaerdierFindes <- TRUE

        cat("I datasættet", datasaetNavn, "findes der værdier, som ikke findes blandt standard/tilladte værdier for variablen/variablene:\n")
        cat(paste0("- ", unique(meltedDatasaet[meltedDatasaet$vaerdiFindesIkkeIListen == 1,]$variable)), sep = "\n")
        cat("En tabel over de værdier der ikke findes i værdisættet for ovenstående variable findes i filen:\n", mdlFil, "\n")


        # Da der ikke findes nogen NA'er i meltedDatasaet bruger jeg aggregate til at lave en tabel over ikke tilladte værdier i variablene
        tabelMdl <- aggregate(vaerdiFindesIkkeIListen~vaerdi+variable, data = meltedDatasaet, FUN = sum) %>%
          filter(vaerdiFindesIkkeIListen != 0) %>%
          arrange(desc(vaerdiFindesIkkeIListen)) %>%
          mutate(antal = vaerdiFindesIkkeIListen,
                 andelAfVariablen = procent(antal, nrow(datasaet))) %>%
          select(-vaerdiFindesIkkeIListen)

        # Definerer filen tabellen skal udskrives til
        mdlFilY <- paste0(mdlFilx, " - ", tjekModVaerdilisten, ".pdf")

        # Skriver i rapporten, hvor tabellen ligger
        cat("Manglerne i", datasaetNavn, "datasættet kan ses i filen:\n", mdlFil, "\n")
        if(udskrivTekstTilRapport == TRUE){sink()}

        # Udskriver tabellen
        udskrivTabelTilPdf(tabelMdl, mdlFilY, titelTekst = paste0("Ikke tilladte værdier for variable med værdilisten ", tjekModVaerdilisten))


        # # Åbner pdf-filen og udskriver tabellen
        # pdf(paste0(mdlFilx, " - ", tjekModVaerdilisten, ".pdf"), width = 8.5, height = 11)
        #
        # # Beregner antallet af sider tabellen fylder
        # antalsider <- ceiling(nrow(tabelMdl) / 35)
        #
        # for(i in 1:antalsider){
        #
        #   # Deler tabellen op efter sidetal
        #   if(i == 1){
        #
        #     tabellen <- tableGrob(tabelMdl[1:min(35, nrow(tabelMdl)),], rows = NULL, theme = tTema)
        #
        #   } else if(i == antalsider){
        #
        #     tabellen <- tableGrob(tabelMdl[((i-1)*35+1):nrow(tabelMdl),], rows = NULL, theme = tTema)
        #
        #   } else {
        #
        #     tabellen <- tableGrob(tabelMdl[((i-1)*35+1):(i*35),], rows = NULL, theme = tTema)
        #
        #   }
        #
        #   # Gør titlen klar efter om det er første eller en af de efterfølgende sider
        #   if(i == 1){
        #
        #     tabelTitel <- textGrob(paste0("Ikke tilladte værdier for variable med værdilisten ", tjekModVaerdilisten), y = unit(1.2, "lines"), vjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
        #
        #   } else {
        #
        #     tabelTitel <- textGrob(paste0("Ikke tilladte værdier for variable med værdilisten ", tjekModVaerdilisten, " - Fortsat"), y = unit(1.2, "lines"), vjust = 0, gp = gpar(fontsize = 12, fontface = "bold"))
        #
        #   }
        #
        #   # Gruppere titel og tabel sammen
        #   tilUdskrift <- arrangeGrob(tabellen, top = tabelTitel, padding = unit(2.6, "lines"))
        #
        #   grid.newpage()
        #
        #   # Udksriver tabellen samt titlen
        #   grid.draw(tilUdskrift)
        #
        # }
        #
        # # Lukker pdf-filen
        # dev.off()

      } else {

        cat("Alle værdier i den oprindelige datasæt for variable med vaerdilisten", tjekModVaerdilisten, "findes blandt standard/tilladte værdier.\n")

      }

    }

  } else {

    cat("Der findes ingen variable med værdiliste.\n")

  }

  if(udskrivTekstTilRapport == TRUE){sink()}

  # Melder nu fejl hvis dette er ønsket og der er fundet mangler/ikke tilladte værdier
  if(meldFejlVedMangler | meldFejlVedIkkeTilladteVaerdier){

    if(udskrivTekstTilRapport == TRUE){sink(file = sti, append = TRUE)}

    # Tjekker om der mangler noget
    if(any(datasaetMangler$bemaerk != "")){# sum(!is.na(datasaetMangler$bemaerk)) != 0

      cat("Fejl: Der er mangler i ", datasaetNavn, ", som overskrider den tilladte andel af mangler.
      For yderligere information se venligst:\n",
          mdlFil, "\n", sep = "")

    }

    # Tjekker om der er ikke tilladte værdier
    if(ikkeTilladteVaerdierFindes){

      cat("Fejl: Der findes ikke tilladte værdier i mindst en af variablene i ", datasaetNavn, ".
      For yderligere information se ovenstående tekst og de tilhørende pdf-fil(er).\n", sep = "")

    }

    if(udskrivTekstTilRapport == TRUE){sink()}

    if(any(datasaetMangler$bemaerk != "") | ikkeTilladteVaerdierFindes){

      # Stopper koden
      stop(paste0("Fejl: Der findes mangler eller ikke tilladte værdier i variablene for", datasaetNavn, ".
        For yderligere information se rapporten i rapportmappen."))

    }

  }

}





### Funktion til at gennemse om datasættet indeholder unikke værdier for de variable, der skal være unikke ###
# Funktionen bruges til at se om en liste af variable der hver især skal indeholde unikke værdier gennem hele datasættet,
# indeholder unikke værdier. Er dette ikke tilfældet meldes der fejl som default.
# Funktionenens resultater skal ikke nødvendigvis skrives til en rapport, men det er muligt at gøre dette ved at definere stien.
#' Inputvalidering (Unikke værdier)
#' @encoding UTF-8
#'
#' @description Funktionen bruges til at se om en liste af variable der hver især skal indeholde unikke værdier gennem hele datasættet,
#'     indeholder unikke værdier. Er dette ikke tilfældet meldes der fejl som default.
#'     Funktionenens resultater skal ikke nødvendigvis skrives til en rapport, men det er muligt at gøre dette ved at definere stien.
#'
#' @param datasaet en data.frame, Datasættet som skal gennemses
#' @param datasaetUnikkeVariableListe en character vektor, En liste over de variable der skal være unikke
#' @param sti en chracter string, Stien til den rapport, som outputtet af funktionen skal sendes til. Rapporten skal være en txt-fil.
#' @param datasaetNavn en chracter string, Brugerdefineret navn til at definere datasættet i rapporten. Hvis NA (default) bruges objektnavnet på datasættet fra miljøet.
#' @param meldFejl TRUE/FALSE værdi, Hvis TRUE melder fejl, hvis de variable der skal være unikke ikke indeholder unikke-værdier
#'
#'
#' @return Tekst med information om hvorvidt de variable i datasættet som skal være unikker er unikke.
#'
#' @export
#'
#'
inputvalUnikkeVaerdier <- function(datasaet, # Datasættet som skal gennemses
                                   datasaetUnikkeVariableListe,# En liste over de variable der skal være unikke
                                   sti = NA,# En sti til den rapport hvor output skal sendes til
                                   datasaetNavn = NA,# Brugerdefineret navn til at definere datasættet - default er at navnet på datasættet i miljøet bruges
                                   meldFejl = TRUE# Hvis TRUE melder fejl hvis de variable der skal være unikke ikke indeholder unikke-værdier
){

  # Definerer navnet på datasættet medmindre denne er angivet som input
  if(is.na(datasaetNavn)){

    datasaetNavn <- deparse(substitute(datasaet))

  }

  ### Gennemser værdierne for alle de variable, hvor der ønskes en test af værdier

  # Danner en kopi af datasaet
  datasaetMdl <- datasaet

  # Laver variablelisten om til en data.frame
  datasaetUnikkeVariableListeDataFrame <- data.frame(unikVariabelNavn = unique(datasaetUnikkeVariableListe), stringsAsFactors = FALSE)

  # Danner et signal til at melde fejl om at der findes variable med ikke unikke værdier
  datasaetUnikkeVariableListeDataFrame$ikkeUnikkeVaerdierFindes <- FALSE

  # Tjekker nu for hver variabel om denne indeholder unikke værdier
  for(tjekVariablen in datasaetUnikkeVariableListeDataFrame$unikVariabelNavn){

    if(nrow(unique(datasaetMdl[tjekVariablen])) !=  nrow(datasaetMdl)){

      datasaetUnikkeVariableListeDataFrame[datasaetUnikkeVariableListeDataFrame$unikVariabelNavn == tjekVariablen,]$ikkeUnikkeVaerdierFindes <- TRUE

    }

  }

  if(!is.na(sti)){

    sink(file = sti, append = TRUE)

  }

  cat("Følgende variable i ", datasaetNavn, " er tjekket for om de kun indeholder unikke værdier:\n", sep = "")
  cat(paste0("- ", datasaetUnikkeVariableListeDataFrame$unikVariabelNavn), sep = "\n")

  if(any(datasaetUnikkeVariableListeDataFrame$ikkeUnikkeVaerdierFindes)){

    cat("Følgende variable indeholder ikke unikke værdier:\n", sep = "")
    cat(paste0("- ", datasaetUnikkeVariableListeDataFrame[datasaetUnikkeVariableListeDataFrame$ikkeUnikkeVaerdierFindes,]$unikVariabelNavn), sep = "\n")

  } else {

    cat("Alle ovenstående variabler indeholder unikke værdier.\n", sep = "")

  }

  if(!is.na(sti)){

    sink()

  }

  # Melder nu fejl hvis dette er ønsket og der er fundet variabler med ikke unikke værdier blandt de variabler der blev tjekket
  if(meldFejl & any(datasaetUnikkeVariableListeDataFrame$ikkeUnikkeVaerdierFindes)){

    if(!is.na(sti)){

      sink(file = sti, append = TRUE)

      cat("Fejl: Variable ( i ", datasaetNavn, "), som bør indeholde kun unikke værdier, indeholder ikke unikke værdier.\n", sep = "")

      sink()

    }

    # Stopper koden
    stop(paste0("Fejl: Variable ( i ", datasaetNavn, "), som bør indeholde kun unikke værdier, indeholder ikke unikke værdier."))

  }

}
