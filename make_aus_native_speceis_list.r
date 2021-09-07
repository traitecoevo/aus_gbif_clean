apc<-read_csv("raw_data/apc/APC-taxon-2021-06-08-0734.csv")
apcs<-filter(apc,taxonRank=="Species"&nameType=="scientific",taxonomicStatus=="accepted")

apcs$nat_count<-str_count(apcs$taxonDistribution,"naturalised")
apcs$comma_count<-str_count(apcs$taxonDistribution,"\\,")

plot(apcs$nat_count,apcs$comma_count)

apcs$native_somewhere_index<-apcs$nat_count<=apcs$comma_count

hmm<-which(apcs$nat_count<=apcs$comma_count)

apcs$taxonDistribution[hmm][1]
apcs$nat_count[hmm][1]
apcs$comma_count[hmm][1]

sum(apcs$native_somewhere_index,na.rm=T)
dim(apcs)

apcs %>%
    mutate(aus_native=apcs$native_somewhere_index) %>%
    select(scientificName,canonicalName,aus_native) %>%
    write_csv("aus_native_lookup.csv")
