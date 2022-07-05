package scalatron.botwar.botPlugin.juskaj

import scala.collection.mutable._
import scala.collection.immutable.Map

class ControlFunction {
    def respond(input: String): String = {
        val (opcode, paramMap) = CommandParser(input)
        if( opcode == "React" ) {
            var bot = Bot(paramMap)
            bot.generateResponse()
            //"Status(text=Cell matrix: "+ neighbors.cellObject +" )|Log(text= + neighbors + )|Move(direction=" + nearestPos.x + ":" + nearestPos.y + ")"
        } else ""
    }
}

case class Bot(params : Map[String,String]){
    var generation = 0
    var response = new StringBuilder()
    var energy = 0
    var time = 0
    var view : View =_
    var name = ""
    
    var allSets : Map[String, String] = Map[String, String]()
    
    def addResponse(action : String) = {
        if(response.length <= 0){
            response.append(action)
        }
        else{
            response.append("|" + action)
        }
    }
    
    def explode(size : Integer) = {
        addResponse(s"Explode(size=$size)")
    }
    
    def move(dir : XY) = {
        val x = dir.x
        val y = dir.y
        addResponse(s"Move(direction=$x:$y)")
    }
    
    def spawn(pos : XY, botType : String, energy : Integer) = {
        val x = pos.x
        val y = pos.y
        addResponse(s"Spawn(direction=$x:$y,name=$botType,energy=$energy)")
    }
    
    def set(params : Map[String, String]) = {
        params.foreach(pair => allSets += pair) 
    }
    
    def generateAllSets() = {
        val allSetsString = allSets.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
        addResponse(allSetsString)
    }
    
    def generateResponse() = {
        view = View(params("view"))
        view.getLinkedView
        generation = params("generation").toInt
        energy = params("energy").toInt
        time = params("time").toInt
        name = params("name")
        
        generation match{
            case 0 => generateResponseForMaster()
            case _ => generateResponseForMiniBot()
        }
        
        response.toString
    }
    
    def generateResponseForMaster() = {
        var lastPosString = params.getOrElse("lastPos", "0:0")
        val neighbors = PathFinder(view, 'P', 'B', 's')
        var nearestPos = neighbors.pathTo.dequeue
        if(!neighbors.pathTo.isEmpty){            
            nearestPos = neighbors.pathTo.dequeue
        }
        move(nearestPos)
        val ls = XY(0,0)
        val last : XY = ls.fromString(lastPosString)
        val lastPos = last.oppositeCords()
        var spawned = false
        val lastMineSpawn = params.getOrElse("lastMineSpawn", "0").toInt
        val lastCollectorSpawn = params.getOrElse("lastCollectorSpawn", "0").toInt
        
        if(time - lastMineSpawn > 200 && energy > 1000){
            spawn(lastPos, "Mine", 500)
            set(Map("lastMineSpawn" -> time.toString))
            spawned = true
        }
        
        if(energy > 1000){
            spawn(lastPos, "Collector", 200)
            set(Map("lastCollectorSpawn" -> time.toString))
            spawned = true
        }
        
        set(Map("lastPos" -> nearestPos.toString))
        generateAllSets()
    }
    
    def generateResponseForMiniBot () = {
        
        name match{
            case "Mine" =>  generateResponseForMine()
            case "Collector" => generateResponseForCollector()
            case _ => false
        }
        

    }
    
    def generateResponseForMine () = {
        var readyToExplode = false
        for(y <- view.size/2 - 4 until view.size / 2 + 4){
            for(x <- view.size/2 - 4 until view.size / 2 + 4){
                view.cells.charAt(x + y * view.size) match{
                    case 'b' | 'B' | 's' | 'm' => readyToExplode = true
                    case _ => false
                }
            }
        }
        if(readyToExplode){        
            explode(4)
        }
    }
    
    def generateResponseForCollector () = {
        
        val masterdir = params("master")
        val matrix = view.cellMatrix
        
        if(energy < 3000){
            var neighbors = PathFinder(view, 'P','B')
            var nearestPos = neighbors.pathTo.dequeue
            if(!neighbors.pathTo.isEmpty){            
                nearestPos = neighbors.pathTo.dequeue
                move(nearestPos)
            }
        }
        
        if(energy > 3000){
            var nearMain = false
            for(y <- 0 until view.size){
                for(x <- 0 until view.size){
                    val currentCell = matrix(y)(x)
                    if(currentCell.cellObject.equals('M')){
                        nearMain = true
                    }
                }
            }
            if (nearMain){
                var neighbors = PathFinder(view, 'M')
                var nearestPos = neighbors.pathTo.dequeue
                if(!neighbors.pathTo.isEmpty){            
                    nearestPos = neighbors.pathTo.dequeue
                    move(nearestPos)
                }
            }
            else{
                var neighbors = PathFinder(view, 'P','B')
                var nearestPos = neighbors.pathTo.dequeue
                if(!neighbors.pathTo.isEmpty){            
                nearestPos = neighbors.pathTo.dequeue
                move(nearestPos)
            }
            }
        }
    }
}

case class View(cells: String) {
    val size = math.sqrt(cells.length).intValue
    val cellMatrix = {
        val arr = Array.ofDim[Cell](size, size)
        for(y <- 0 until size){
            for(x <- 0 until size){
                arr(y)(x) = Cell(XY(x,y), cells.charAt(x + y * size))
            }
        }
        arr
    }
    val masterCell = cellMatrix(size/2)(size/2)
    def getLinkedView() = {
        val matrix = cellMatrix
        for(y <- 0 until size){
            for(x <- 0 until size){
                val currentCell = matrix(y)(x)
                if (x > 0){
                    val leftCell = matrix(y)(x - 1)
                    leftCell.right = currentCell
                    currentCell.left = leftCell
                    
                    if (y > 0){
                        val leftTopCell = matrix (y - 1)(x - 1)
                        leftTopCell.rightDown = currentCell
                        currentCell.leftTop = leftTopCell
                    }
                }
                if (y > 0){
                    val upCell = matrix(y - 1)(x)
                    upCell.down = currentCell
                    currentCell.up = upCell
                    
                    if(x < size - 1){
                        val rightTopCell = matrix (y - 1)(x + 1)
                        rightTopCell.leftDown = currentCell
                        currentCell.rightTop = rightTopCell
                    }
                }     
            }
        }
    }
}

object PathFinder{
    def apply(view : View, goodCells : Char*) = {
        
        def goodCell(cell : Cell) = {
        if (!cell.pathTo.isEmpty) {           
            var isGood = false
            goodCells.foreach(good => {if (cell.cellObject.equals(good)) isGood = true})
            isGood
        }
        else{        
            false
        }
    }
        
        var cellQueue = Queue[Cell]()
        val cellMatrix = view.cellMatrix 
        
        var visited = Array.fill(view.cells.length)(false)
        
        val botCell = view.masterCell
        botCell.pathTo.enqueue(XY(0,0))
        cellQueue.enqueue(botCell)
        visited(botCell.pos.x + botCell.pos.y * view.size) = true
        
        var steps = 0
        
        while (!cellQueue.isEmpty){
            
            var cell = cellQueue.dequeue           
                    for ((key, value) <- cell.existingNeighbors){                       
                            if (!visited(key.pos.x + key.pos.y * view.size)){
                                visited(key.pos.x + key.pos.y * view.size) = true;
                                key.pathTo = cell.pathTo.clone()
                                key.pathTo.enqueue(value)
                                cellQueue.enqueue(key)
                            }
                }   
            
            //cell.existingNeighbors.foreach(pair => {if (!visited(pair._1.pos.x + pair._1.pos.y * view.size)){ visited(pair._1.pos.x + pair._1.pos.y * view.size) = true; cellQueue.enqueue(pair._1)}})
            steps += 1
        }
        
        
        var min = view.size + 1
        var max = 0
        var matrix = view.cellMatrix
        var nearestCell : Cell = botCell;
        var furthestCell : Cell = botCell;
        
        for(y <- 0 until view.size){
            for(x <- 0 until view.size){
                val cl = matrix(x)(y)
                if(goodCell(cl)){
                    if(cl.pathTo.size < min){
                        min = cl.pathTo.size
                        nearestCell = cl
                    }
                }
                if(cl.pathTo.size > max){
                    max = cl.pathTo.size
                    furthestCell = cl
                }
            }
        }
        
        if(nearestCell == botCell){
            furthestCell
        }
        else{        
            nearestCell
        }
    }
}

case class Cell(pos : XY, cellObject : Char){
    var pathTo : Queue[XY] = Queue[XY]();
    var up : Cell = _;
    var down : Cell = _;
    var left : Cell = _;
    var right : Cell = _;
    var leftTop : Cell = _;
    var leftDown : Cell = _;
    var rightTop : Cell = _;
    var rightDown : Cell = _;
    
    def availableCell(cell : Cell) = {
        if (cell != null) {
            cell.cellObject match {
                case '_' | 'P' | 'B' | 's' | 'M' => true
                case _ => false
            }
        }
        else{        
            false
        }
    }
    
    def existingNeighbors() = {
        var cellMap = Map[Cell, XY]()
        
        if (availableCell(up)){
            cellMap += (up -> XY(0,-1))
        }
        if (availableCell(down)){
            cellMap += (down -> XY(0,1))
        }
        if (availableCell(left)){
            cellMap += (left -> XY(-1,0))
        }
        if (availableCell(right)){
            cellMap += (right -> XY(1,0))
        }
        if (availableCell(leftTop)){
            cellMap += (leftTop -> XY(-1,-1))
        }
        if (availableCell(leftDown)){
            cellMap += (leftDown -> XY(-1,1))
        }
        if (availableCell(rightTop)){
            cellMap += (rightTop -> XY(1,-1))
        }
        if (availableCell(rightDown)){
            cellMap += (rightDown -> XY(1,1))
        }
        
        cellMap
    }
}

case class XY(x: Int, y: Int){
    override def toString() = {
        var sb = new StringBuilder("")
        val str = sb.append(s"$x:$y")
        str.toString
    }
    
    def fromString(numbers : String) = {
        val xy = numbers.split(':')       
        val newXY : XY = XY(xy(0).toInt,xy(1).toInt)
        newXY
    }
    
    def oppositeCords() = {
        var newx = 0
        var newy = 0
        if(x == 1){
            newx = -1
        }
        else if (x == -1){
            newx = 1
        }
        if (y == 1){
            newy = -1
        }
        else if (y == -1){
            newy = 1
        }
        
        val newXY : XY = XY(newx, newy)
        newXY
    }
}

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}