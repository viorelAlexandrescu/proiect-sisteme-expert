package application;
	
import javafx.application.Application;
import javafx.stage.Stage;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.fxml.FXMLLoader;


public class Main extends Application {

    private static Parent rootParent;
    private static Stage mainStage;

    @Override
    public void start(Stage primaryStage){
        
        try {
        	rootParent = (Pane)FXMLLoader.load(getClass().getResource("MainWindow.fxml"));
			Scene scene = new Scene(rootParent);
			scene.getStylesheets().add(getClass().getResource("application.css").toExternalForm());
		
			mainStage = primaryStage;
			mainStage.setTitle("Sistem Exp");
			mainStage.setScene(scene);
			mainStage.show();
		} catch(Exception e) {
			System.out.println(e.getMessage());
            e.printStackTrace();
            System.out.println("Please try again later");
            System.exit(1);
		}
    }

    static Parent getRootParent() {
        return rootParent;
    }

    static Stage getMainStage() {
        return mainStage;
    }

    @Override
    public void stop() throws Exception{
        System.out.println("App closing...");
        // close connection 
        System.out.println("Connection to DB closed.");
        System.out.println("Bye");
    }
    
	public static void main(String[] args) {
		launch(args);
	}
}
