import Network.HTTP
import Text.HTML.TagSoup
import List
import qualified Data.HashMap as HM
isMeaningfulURL url = ("http://tvtropes.org/pmwiki/pmwiki.php/" `isPrefixOf` url) && (elemIndex '?' url == Nothing)
extractURLs = map (fromAttrib "href").filter (isTagOpenName "a")
discardBeforeWikiText = snd.break (\t->(isTagOpenName "div" t) && (fromAttrib "id" t=="wikitext"))
grabAllMeaningfulURLs = (filter isMeaningfulURL.extractURLs)
grabAllMeaningfulURLsInWikiText = (grabAllMeaningfulURLs.discardBeforeWikiText)

test0 = simpleHTTP (getRequest "http://tvtropes.org/pmwiki/pmwiki.php/Main/HomePage") >>= getResponseBody >>= writeFile "test0.txt".unlines.grabAllMeaningfulURLs.parseTags
testGet url = simpleHTTP (getRequest url) >>= getResponseBody >>= return.parseTags
testGetMain = testGet "http://tvtropes.org/pmwiki/pmwiki.php/Main/HomePage"
testGetKnK = testGet "http://tvtropes.org/pmwiki/pmwiki.php/Anime/KaraNoKyoukai"
test url = testGet url>>=writeFile "test.txt".unlines.grabAllMeaningfulURLsInWikiText

entryURL = "http://tvtropes.org/pmwiki/pmwiki.php/Main/Tropes"

prettyShowTerm (v,es) = unlines (v:map ('\t':) es)

prettyShowMap = unlines.map prettyShowTerm.HM.toList
prettyReadMap = parse HM.empty.lines where
	parse m [] = m
	parse m ("":us) = parse m us
	parse m (u:us) = parse (HM.insert u (map tail $ fst pr) m) (snd pr) where pr = span valid us 
	valid ('\t':s) = True
	valid _ = False
	
crawlFrom depth url known= 
	if HM.member url known 
	then return known
	else testGet url>>=( if depth==0 then return.g else \us->f us (g us) ).grabAllMeaningfulURLsInWikiText where
		g urlList = HM.insert url urlList known
		f [] map = return map
		f (u:us) map = crawlFrom (depth-1) u map>>=f us
		--f (u:us) map = if( url == u ) then crawlFrom (depth-1) u map>>=f us else f us map
		--f (u:us) map = if( "NarrativeTropes" `isSuffixOf` u ) then crawlFrom (depth-1) u map>>=f us else f us map
		--f [] = return
		--f (u:us) = crawlFrom (depth-1) u>>=f us

crawlByList crawlF [] known = return known
crawlByList crawlF (u:us) known = crawlF u known>>=crawlByList crawlF us
--main = crawlFrom 0 entryURL HM.empty >>= writeFile "tvtropes.txt".prettyShowMap
--main = readFile "entryList.txt">>=(\s->crawlByList (crawlFrom 2) (lines s) HM.empty)>>=writeFile "tvtropes.txt".prettyShowMap
main = crawlFrom 1 "http://tvtropes.org/pmwiki/pmwiki.php/Main/Anime" HM.empty >>= writeFile "Tropes.txt".prettyShowMap
