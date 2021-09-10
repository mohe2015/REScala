package de.ckuessner
package todolist

import scalafx.Includes._
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ListView, TextField}
import scalafx.scene.layout.{HBox, Priority, VBox}

import java.util.UUID

object TodoListApp extends JFXApp3 {
  override def start(): Unit = {
    val todoListView = new ListView[UUID] {
      cellFactory = { listView => new TodoItemListCell() }
      items = TodoListController.observableUuidList
    }

    val newTodoTextField = new TextField {
      promptText = "Todo Entry"
      hgrow = Priority.Always
    }

    val addTodoButton = new Button {
      text = "+"
      onAction = () => {
        TodoListController.addTodo(TodoEntry(newTodoTextField.text.value))
        newTodoTextField.clear()
      }
    }

    val connectionTextField = new TextField {
      promptText = "replica@remote"
      hgrow = Priority.Always
    }

    val localAddressTextField = new TextField {
      text = TodoListController.connectionString
      editable = false
    }
    localAddressTextField.focusedProperty.addListener(_ =>
      Platform.runLater {
        if (localAddressTextField.isFocused) {
          localAddressTextField.selectAll()
        }
      }
    )

    val addConnectionButton = new Button("Connect")
    addConnectionButton.onAction = () => {
      val connectionString = connectionTextField.getText
      if (connectionString.isBlank) return
      TodoListController.connect(connectionString)
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = s"Encrdt Todolist (replica ${TodoListController.replicaId})"
      scene = new Scene {
        content = new VBox {
          children = Seq(
            todoListView,
            new HBox {
              children = Seq(newTodoTextField, addTodoButton)
            },
            new HBox {
              children = Seq(connectionTextField, addConnectionButton)
            },
            localAddressTextField,
            new HBox {
              children = Seq(
                new Button {
                  text = "Log state"
                  onAction = () => Console.println(s"State: ${TodoListController.todos})")
                },
                new Button {
                  text = "Log peers"
                  onAction = () => Console.println(s"Peers: ${TodoListController.peers})")
                }
              )
            }
          )
        }
      }
    }
  }

  override def stopApp(): Unit = {
    TodoListController.stop()
  }
}