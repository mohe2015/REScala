package todo

import rescala.default._
import kofre.decompose.Delta
import rescala.extra.lattices.delta.crdt.reactive.RGA
import rescala.extra.lattices.delta.crdt.reactive.RGA._
import todo.Todolist.replicaId

import java.util.concurrent.ThreadLocalRandom

class TaskOps(taskRefs: TaskRefObj) {

  type State = RGA[TaskRef]

  def listInitial: State = RGA[TaskRef](replicaId)

  def handleCreateTodo(state: => State)(desc: String): State = {
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskRefs.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    state.resetDeltaBuffer().prepend(taskref)
  }

  def handleRemoveAll(state: => State, dt: DynamicTicket): State = {
    state.resetDeltaBuffer().deleteBy { taskref =>
      val isDone = dt.depend(taskref.task).read.exists(_.done)
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (isDone) taskref.task.disconnect()
      isDone
    }
  }

  def handleRemove(state: => State)(id: String): State = {
    state.resetDeltaBuffer().deleteBy { taskref =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (delete) taskref.task.disconnect()
      delete
    }
  }

  def handleDelta(s: => State)(delta: Delta[RGA.State[TaskRef]]): State = {
    val list = s

    val newList = list.resetDeltaBuffer().applyDelta(delta)

    val oldIDs = list.toList.toSet
    val newIDs = newList.toList.toSet

    val removed = oldIDs -- newIDs
    removed.foreach { _.task.disconnect() }

    newList
  }

}
