import Network.HTTP.Conduit
import Data.List
import Data.ByteString.Char8 as BS (concat, unpack, ByteString)
import Data.ByteString.Lazy as LBS (toChunks, ByteString)
import Data.List.Split

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

main = do
	putStrLn "Posts density project. a_kiselev, 2013"
	putStrLn "@19299070"
	putStrLn "Enter posts count(max=100):"
	count <- getLine
	putStrLn "Group walls begins with '-'. Enter wall id:"
	id <- getLine
	request' <- simpleHttp $ "https://api.vk.com/method/wall.get?owner_id=" ++ id ++ "&offset=0&count=" ++ count ++ "&filter=owner&extended=1"
	let request = unpack $ lazyToStrictBS request'
	let postDates = map dateToDays $ map (read::(String -> Integer)) $ tail $ map takeData $ splitOn "\"id\":" request
	let interval = fromIntegral (head postDates - last postDates)
	putStrLn $ ("This user posted "++ count ++" posts in " ++ show interval ++ " days. Average is "
		 ++ show (rounding $ ((read count)::Double) / interval) ++ " posts per day.")


takeData ('\"':'d':'a':'t':'e':'\"':_:xs) = take 10 xs
takeData (x:xs) = takeData xs
takeData [] = []
--86400
dateToDays :: Integer -> Integer
dateToDays date 
	| date > 86400 = 1 + (dateToDays $ date - 86400)
	| otherwise = 0
		
rounding :: Double -> Double
rounding number = (fromIntegral $ round $ number * 10000) / 10000

{-

	{"response":
	
	{"wall":
	[1076,
	{"id":3200,"from_id":19299070,"to_id":19299070,"date":1385659765,"post_type":"post","text":"","attachment":
	{"type":"photo","photo":
	{"pid":316119586,"aid":-7,"owner_id":19299070,"src":"http:\/\/cs419825.vk.me\/v419825070\/7a7b\/UoC9HVvnjjk.jpg","src_big":"http:\/\/cs419825.vk.me\/v419825070\/7a7c\/-bm9O-9GxhE.jpg","src_small":"http:\/\/cs419825.vk.me\/v419825070\/7a7a\/5wwjSGOgaNo.jpg","src_xbig":"http:\/\/cs419825.vk.me\/v419825070\/7a7d\/UojQebmgVoQ.jpg","width":610,"height":320,"text":"","created":1385659762,"post_id":3200,"access_key":"062db0bd30f852a175"}},"attachments":
	[{"type":"photo","photo":
	{"pid":316119586,"aid":-7,"owner_id":19299070,"src":"http:\/\/cs419825.vk.me\/v419825070\/7a7b\/UoC9HVvnjjk.jpg","src_big":"http:\/\/cs419825.vk.me\/v419825070\/7a7c\/-bm9O-9GxhE.jpg","src_small":"http:\/\/cs419825.vk.me\/v419825070\/7a7a\/5wwjSGOgaNo.jpg","src_xbig":"http:\/\/cs419825.vk.me\/v419825070\/7a7d\/UojQebmgVoQ.jpg","width":610,"height":320,"text":"","created":1385659762,"post_id":3200,"access_key":"062db0bd30f852a175"}}]
	,"comments":
	{"count":0},"likes":
	{"count":0},"reposts":
	{"count":0}}],
	"profiles":
	[{"uid":19299070,"first_name":"Антон","last_name":"Киселёв","sex":2,"screen_name":"a_kiselev","photo":"http:\/\/cs313321.vk.me\/v313321070\/31a7\/W-L1ykhCNDM.jpg","photo_medium_rec":"http:\/\/cs313321.vk.me\/v313321070\/31a6\/9kSUpp4s7qc.jpg","online":1}],"groups":
	[]}}

-}
	
