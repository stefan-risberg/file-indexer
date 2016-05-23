import QtQuick 2.0
import QtQuick.Controls 1.3
import QtQuick.Dialogs 1.1

Rectangle {
    id: mainWindow
    width: 640
    height: 480

    FileDialog {
        id: folderSelectDialog
        title: "Choose folder to scan"
        folder: shortcuts.home
        selectFolder: true
        onAccepted: dirScan.text = cleanPath(folderSelectDialog.fileUrl)
    }

    TextField {
        id: inputField
        placeholderText: qsTr("Text Field")
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.leftMargin: 4
        anchors.right: selectBtn.left
        anchors.rightMargin: 4
    }

    Button {
        id: selectBtn
        text: qsTr("Select")
        anchors.top: parent.top
        anchors.right: parent.right
    }

    TextArea {
        id: outputArea
        anchors.top: inputField.bottom
        anchors.topMargin: 4
        anchors.bottom: scanBtn.top
        anchors.bottomMargin: 4
        anchors.right: selectBtn.right
        anchors.left: inputField.left
    }

    Button {
        id: scanBtn
        text: qsTr("Scan")
        anchors.bottom: parent.bottom
        anchors.right: parent.right
    }
}
