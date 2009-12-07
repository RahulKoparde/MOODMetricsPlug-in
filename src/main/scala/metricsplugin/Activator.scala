package metricsplugin

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
class Activator extends AbstractUIPlugin {
  
 /*
  * (non-Javadoc)
  * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
  */
 override def start(context : BundleContext) : Unit = {
   super.start(context);
   Activator.plugin = this;
 }

 /*
  * (non-Javadoc)
  * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
  */
  override def stop(context : BundleContext) : Unit = {
		Activator.plugin = null;
		super.stop(context);
  }
}

object Activator {
  // The plug-in ID  
  val PLUGIN_ID : String = "MOODMetricsPlugin";
  
  // The shared instance
  private var plugin : Activator = null
 
  /**
   * Returns the shared instance
   *
   * @return the shared instance
   */  
  def getDefault() : Activator = {
    return plugin
  }

  /**
   * Returns an image descriptor for the image file at the given
   * plug-in relative path
   *
   * @param path the path
   * @return the image descriptor
   */  
  def getImageDescriptor(path : String) : ImageDescriptor = {
    return AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN_ID, path);
  }
}

