import QtQuick 2.0

Column {
    height: 300;
    Rectangle {
        color: "red";
        width: childrenRect.width;
        height: childrenRect.height;

        Text {
            width: 300;
            height: 30;
            font.pixelSize: 30;
            text: "Hello World.";
            color: "white";
        }

        MouseArea {
            anchors.fill: parent;
            onClicked: output.text = helloworld ();
        }
    }

    Text {
        width: 300;
        wrapMode: Text.WrapAnywhere; font.pixelSize: 30;
        id: output
import QtQuick 2.0

Column {
    height: 300;
    Rectangle {
        id: rectangle1
        color: "red";
        width: childrenRect.width;
        height: childrenRect.height;

        Text {
            width: 300;
            height: 30
            font.pixelSize: 30;
            text: "Hello World.";
            verticalAlignment: Text.AlignVCenter
            transformOrigin: Item.Center
            color: "white";
        }

        MouseArea {
            anchors.fill: parent;
            onClicked: output.text = helloworld ();
        }
    }

    Text {
        width: 300;
        wrapMode: Text.WrapAnywhere; font.pixelSize: 30;
        id: output
    }
}

    }
}

