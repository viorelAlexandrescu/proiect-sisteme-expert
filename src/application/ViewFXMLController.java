package application;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.stage.Stage;

public class ViewFXMLController implements Initializable {

	private static Parent secondParent;
	private static Stage secondStage = new Stage();

	public static Parent getSecondParent() {
		return secondParent;
	}

	public static void setSecondParent(Parent secondParent) {
		ViewFXMLController.secondParent = secondParent;
	}

	public static Stage getSecondStage() {
		return secondStage;
	}

	public static void setSecondStage(Stage secondStage) {
		ViewFXMLController.secondStage = secondStage;
	}

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		System.out.println("Hello world!");
	}
}
