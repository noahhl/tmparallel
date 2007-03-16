stopwords <- local({
      english <- readLines(system.file("stopwords", "english.dat", package = "tm"))
      german <- readLines(system.file("stopwords", "german.dat", package = "tm"))
      french <- readLines(system.file("stopwords", "french.dat", package = "tm"))
      function(language = "en") get(resolveISOCode(language))
   })
