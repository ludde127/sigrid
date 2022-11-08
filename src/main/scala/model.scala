import java.time.Duration

case class User(name: String, number: Int, var timeStartedWaiting: Option[Date] = None){
  require(name == User.validName(name), s"invalid user name: $name")
  require(number > 0, s"invalid user number: $number")
  val id = s"$name-$number"
  override def toString: String = {
    id
  }

  override def equals(x: Any): Boolean = 
    x match {
      case user: User => user.name == name && user.number == number
      case _ => false
    }


  def joinedQueue(): Unit = 
    timeStartedWaiting = Some(Date.now())
  def leaveQueue(): Unit = 
    timeStartedWaiting = None


  def timeWaited(): Option[Duration] = {
    if (timeStartedWaiting.isEmpty) {
      None
    } else {
      Some(Duration.between(timeStartedWaiting.get.dateTime, Date.now().dateTime))
    }
  }

  def waitedMinSafe: Double = timeWaited().getOrElse(Duration.ofMinutes(0)).toSeconds()/60.0

}

object User {
  val DefaultUserName = "oddput"
  val MaxNameLength = 25
 
  def validName(s: String): String = 
    if (s.nonEmpty) s.filter(_.isLetter).take(MaxNameLength).toLowerCase 
    else DefaultUserName
  
  def validUserId(s: String): String = 
    s.filter(c => c.isLetterOrDigit || c == '-').toLowerCase

  def fromUserId(uid: String): Option[User] = scala.util.Try {
    val xs = validUserId(uid).split('-')
    assert(xs.length == 2)
    User(validName(xs(0)),xs(1).toInt)
  }.toOption
}

case class RoomKey private (course: String, roomName: String){
  override def toString = s"<br>&nbsp;RoomKey($course,$roomName)"
}
object RoomKey {
  val MaxCourseLength = 25
  val   MaxRoomLength = 20
  val   DefaultCourse = "EDAA45"
  val     DefaultRoom = "Idét"
  val      knownRooms = 
    "Pluto Neptunus Uranus Saturnus Jupiter Mars Venus Elg Elgkalv Hacke Panter Ravel Val Falk Varg Alfa Beta Gamma Idét Distans".split(" ").toSet

  def roomWarning(room: String): String = 
    if (RoomKey.knownRooms.contains(room)) "" 
    else s"""<p class="blink">VARNING: Rum $room okänt i E-huset. Felstavat?</p>""" + 
         s"<p>Kända rum: ${RoomKey.knownRooms.toSeq.sorted.mkString(", ")}.</p>"


  def validCourse(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetterOrDigit).take(MaxCourseLength).toUpperCase 
    else DefaultCourse
  
  def validRoomName(s: String): String = 
    if (s.nonEmpty) s.filter(c => c.isLetterOrDigit).take(MaxRoomLength).toLowerCase.capitalize 
    else DefaultRoom
  
  def apply(course: String, roomName: String): RoomKey = 
    new RoomKey(validCourse(course), validRoomName(roomName))
}

object Room {
  val HoursUntilExpired = 10
}
case class Room(
  course: String, 
  name: String, 
  supervisors: Set[User] = Set(),
  students: Set[User] = Set(), 
  helpQueue: Vector[User] = Vector(), 
  approvalQueue: Vector[User] = Vector(), 
  created: Date = Date.now(),
){
  def wantHelp(u: User): Room = copy(
    helpQueue = if (helpQueue.contains(u)) helpQueue else {u.joinedQueue(); helpQueue :+ u},
    approvalQueue = approvalQueue.filterNot(_ == u)
  )

  def wantApproval(u: User): Room = copy(
    helpQueue = helpQueue.filterNot(_ == u),
    approvalQueue = if (approvalQueue.contains(u)) approvalQueue else {u.joinedQueue(); approvalQueue :+ u}
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
    supervisors = supervisors - u
  )
  
  def queueWithTimer(vector: Vector[User]): String = {
      if (vector.size >= 1) {
        val headWaited = f"(${vector.head.id}: Väntat ${(vector.head.waitedMinSafe*10).toInt/10.0} min) "
        if (vector.size > 1) {
          headWaited + vector.tail.mkString(", ")
        } else {
          headWaited
        }
      } else {
        ""
      }
    }

  def clearHelpQueue(): Room = copy(helpQueue = Vector())

  def clearApprovalQueue(): Room = copy(approvalQueue = Vector())

  def popHelpQueue(): Room = copy(helpQueue = helpQueue.drop(1))

  def popApprovalQueue(): Room = copy(approvalQueue = approvalQueue.drop(1))

  def isExpired: Boolean = created < Date.now().minusHours(Room.HoursUntilExpired)

  def isActive: Boolean = supervisors.nonEmpty || students.nonEmpty

  def isRemovable: Boolean = !isActive || isExpired 

  override def toString = 
    s"Room($course, $name, supervisor=$supervisors, students=$students), helpQueue=$helpQueue, approvalQueue=$approvalQueue, created=${created})"
}
