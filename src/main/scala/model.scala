case class User(name: String, number: Int){
  require(name == User.validName(name), s"invalid user name: $name")
  require(number > 0, s"invalid user number: $number")
  val id = s"$name-$number"
  override def toString = id
}

object User {
  val DefaultUserName = "oddput"
  val   MaxNameLength = 25
 
  def fromUserId(uid: String): Option[User] = scala.util.Try {
    val xs = uid.split('-')
    assert(xs.length == 2)
    User(validName(xs(0)),xs(1).toInt)
  }.toOption

  def validName(s: String): String = 
    if (s.nonEmpty) s.filter(_.isLetter).take(MaxNameLength).toLowerCase 
    else DefaultUserName
}

case class RoomKey private (course: String, roomName: String)
object RoomKey {
  val MaxCourseLength = 25
  val   MaxRoomLength = 20
  val   DefaultCourse = "KURS01"
  val     DefaultRoom = "Shäraton"

  def validCourse(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetter || c.isDigit).take(MaxCourseLength).toUpperCase 
    else DefaultCourse
  
  def validRoomName(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetter).take(MaxRoomLength).toLowerCase.capitalize 
    else DefaultRoom
  
  def apply(course: String, roomName: String): RoomKey = 
    new RoomKey(validCourse(course), validRoomName(roomName))
}

case class Room(
  course: String, 
  name: String, 
  supervisor: Option[User] = None,
  students: Set[User] = Set(), 
  helpQueue: Vector[User] = Vector(), 
  approvalQueue: Vector[User] = Vector(), 
){
  def wantHelp(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_ == u) :+ u,
    approvalQueue = approvalQueue.filterNot(_ == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u) :+ u
  )

  def working(u: User): Room = copy(
    students = students + u,
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u)
  )

  def goodbye(u: User): Room = copy(
    students = students - u,
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = approvalQueue.filterNot(_ == u),
    supervisor = supervisor.flatMap(s => if (s == u) None else Some(s))
  )

  override def toString = 
    s"Room($course,$name,supervisor=$supervisor,students=$students),helpQueue=$helpQueue,approvalQueue=$approvalQueue"
}