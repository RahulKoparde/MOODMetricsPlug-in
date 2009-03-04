package metricsplugin;

import org.eclipse.core.commands.{AbstractHandler,ExecutionEvent,ExecutionException};
import org.eclipse.jface.dialogs.{InputDialog,MessageDialog}
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import org.eclipse.core.resources.{IProject,IWorkspace,IWorkspaceRoot,ResourcesPlugin};

import org.eclipse.jdt.core._
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.internal.core.PackageFragment;

class MOODMetricHandler extends AbstractHandler {

	override def execute(event: ExecutionEvent) : Object = {
		val window : IWorkbenchWindow = HandlerUtil.getActiveWorkbenchWindowChecked(event);

        val root:IWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
        val workspace:IWorkspace = ResourcesPlugin.getWorkspace();
        val javaModel:IJavaModel = JavaCore.create(workspace.getRoot());
        
        /*
         * doesn't work.  if the project contains the java nature, then it is marked as java project
         */
        val nonJava:Array[Object] = javaModel.getNonJavaResources
        val projects:Array[IJavaProject] = javaModel.getJavaProjects.
          filter(x => !nonJava.exists(_.asInstanceOf[IProject].getName == x.getElementName))
        
		val ip : InputDialog = new InputDialog(window.getShell(), 
				"Select project name",
				"Please look at your package explorer and select a *Java* project.  \n\nE.g., " + 
                projects.map(_.getElementName).reduceLeft{_+", "+_},
				if(projects.size > 1) { projects(0).getElementName } else { "project_from_workspace" },
				null);
		
        def projectExists(name:String) = projects.findIndexOf(_.getElementName == name) != -1
        //projects.map(_.)
        
        
		ip.open();
        val pN:String = ip.getValue
        if(!projectExists(pN)) {
          return null;
        } else { }
  
        val projectName : String = pN        
        
	    val m : MetricsCalculator = new MetricsCalculator(projectName);
				
	    MessageDialog.openInformation(
	      window.getShell(),
	      "Metrics",
	      "Calculating metrics ... please be patient");    
	    
	    val sizeMetrics : String =
	      "LOC: " + m.getLOC + "\n" +
	      "Total Classes: " + m.countClasses + "\n" +
	      "Total Methods: " + m.getTotalMethods + "\n" +
	      "Total attributes: " + m.getTotalFields + "\n";	    
	    
	    val moodMetrics : String = 
	      "AHF: " + m.getAHF + "\n" +
	      "MHF: " + m.getMHF + "\n" +
	      "AIF: " + m.getAIF +"\n" + 
	      "MIF: " + m.getMIF +"\n" + 
	      "CF: " + m.getCouplingFactor +"\n" +
	      "PF: " + m.getPolymorphismFactor +"\n";
	    
	    MessageDialog.openInformation(
	      window.getShell(),
	      "Metrics",
	      "Data Analysis:\n\n" + sizeMetrics + "\n\n" + moodMetrics);
	    
	    return null;
	}

}
