import ujson.*

2+2

val obj = Map("1" -> "Rock", "3"-> "Paper")
obj("1")

val encoded = Arr(obj.map((key,value) => Obj("key" -> key, "value" -> Str(value))))

def decode(json: Value, tag: String) :Map[String,String] = 
    val json_val = json.obj
    tag match 
        case "Selecting" => 
            Map(json_val("key").str -> json_val("value").str)
    
    
val objet = Obj("key" -> Str("1"), "value" -> Num(2))

val test1 = Arr(objet)
test1.arr.toList.head.obj

val test = encoded.arr.toList.head.arr.toList
test.arr.toList.head.obj


encoded.arr.toList.head.arr.map(decode(_,"Selecting")).flatten.toMap

encoded.arr.toList.flatMap(_.arr.toList).map(decode(_,"Selecting")).flatten.toMap


encoded.arr.toList.flatMap(_.arr.toList).map(decode(_,"Selecting")).flatten.toMap

val scoresView = Map("1" -> 2, "2"-> 1)
val json_score = Arr(scoresView.map((key,value) => Obj("key" -> key, "value" -> Num(value))))
json_score.arr.toList.head.arr.toList.map(x => Map(x.obj("key").str -> x.obj("value").num))

val test2 = Map("1" -> "Rock")
val test3 = test2 + ("2" -> "Scissors")
test3.keys
val key = test3.keys
val hands = test2(key.head)

val score = Map("1" -> 0, "2"-> 0, "3" -> 0)
score + ("1" -> (score("1") + 1) )

score.filter(_._1 != "1").map(_._2)

val players = Vector("1","2", "3")
val versusHands = Map("1" -> "Rock", "2" -> "Scissors", "3"-> "Rock")



def fight(str1 : String, str2: String): Int = 
    if str1 == str2 then 1
    else -1

val final_score = 
    for 
        player <- players
        opponents = players.filter(_ != player)
        score = 
            for 
                opponent <- opponents
                res = fight(versusHands(player), versusHands(opponent))
            yield(res)
    yield (player -> score.toList.foldLeft(0)(_+_))


final_score
final_score.map((str,int) => (str -> int)).toMap
