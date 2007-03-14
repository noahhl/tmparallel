stopwords <- local({
      en <- english <- readLines(system.file("stopwords", "english.dat", package = "tm"))
      de <- german <- deutsch <- readLines(system.file("stopwords", "german.dat", package = "tm"))
      fr <- french <- francais <- readLines(system.file("stopwords", "french.dat", package = "tm"))
      function(language = "en") get(sub("_.*", "", language))
   })
