package metricsplugin

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import org.eclipse.core.commands.{AbstractHandler,ExecutionEvent,ExecutionException};
import org.eclipse.core.resources.{IProject,IWorkspace,IWorkspaceRoot,ResourcesPlugin};

import org.eclipse.jdt.core._
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.internal.core.PackageFragment;

import scala.collection.mutable.Map

/**
 * This class implements the MOOD Metrics defined in R. Harrison, S. J. Counsell, R. V. Nithi: 
 * An evaluation of the MOOD set of objectoriented software metrics. IEEE Transactions on 
 * Software Engineering 24(6):491-496. 1998.
 * 
 * @param proj The project which you wish to calculate the metrics for, NB. this has to be a 
 * project within the workspace.  No extra steps are performed to ensure that it is.
 * @author Mohamed Bana
 * 
 */
class MetricsCalculator(proj:String) {
  
  /**
   * The Visibility of a method or field.
   */  
  private type Visibility = String

 /**
   * The Field's name
   */  
  private type FieldName = String
  
 /**
   * The Method's name
   */  
  private type MethodName = String
  
 /**
   * The Class's name 
   */  
  private type ClassName = String
  
 /**
   * Aliasing for a ICompilationUnit
   */  
  private type ICU = ICompilationUnit
  
  /*
   * A mapping from Class names to their methods
   */
  private var methods = Map[ClassName,List[(MethodName,Visibility)]]()
  
  /*
   * Like above, except that it's from Class to their corresponding
   * fields and visibility.
   */
  private var fields = Map[ClassName,List[(FieldName,Visibility)]]()
  
  /* 
   * ICompilationUnit represents the topmost class in a .java file.  Or
   * each source compilation unit.
   */
  private var classes = List[ICompilationUnit]()
  
  /*
   * Each individual class including nested ones.
   */
  private var classTD = List[TypeDeclaration]()

  /*
   * Each individual class including nested ones.
   */
  private var classTypes = List[IType]()
       
  /*
   * Stored for later reference, it's actually quite hard to get this
   */
  private var objectTB : ITypeBinding = null;
  
  /**
   * Determines if constructors are to be counted.  
   * By default we do not count constructor calls.
   */
  private def countConstructors = false
  
  /**
   * Determines if are in debug mode.  
   * By default we do not debug.
   */
  private def debug = false
  
  /*
   * Setups up the fields of the classes etc for performing the analysing, 
   * it's more convenient to store them now as traversing the AST is quite
   * expensive.  On my initial unoptimized dry run, it was taking >30 seconds to
   * the metric even on small projects.
   */  
  setup()

  /** 
   * The total number of methods in the current <code>Project</code>.
   * 
   * @return The total number of methods in the current <code>Project</code>.
   */  
  def getTotalMethods = {
    var x = methods.values.toList.flatMap { x => x } 
    //Console println "getTotalMethods:x " + x
    x.length
  }

  /** 
   * The total number of fields/attributes in the <code>Project</code>.
   * 
   * @return The total number of fields/attributes in the <code>Project</code>.
   */   
  def getTotalFields = {
    var x = fields.values.toList.flatMap { x => x } 
    //Console println "getTotalFields:x " + x
    x.length
  }
  
  /** 
   * The number of Lines Of Code in the current <code>Project</code>.
   * 
   * @return The number of Lines Of Code in the current <code>Project</code>.
   */   
  def getLOC() : Int = {
    var x = 0
    // for all the compilation units, i.e., source files
    for(cu <- classes) {
      var lineT = new org.eclipse.jface.text.DefaultLineTracker()
      x += lineT.computeNumberOfLines(cu.getBuffer.getContents)
    }    
    x
  } 
  
  /** 
   * The total number of classes in the current <code>Project</code>.
   * 
   * @return The total number of classes in the current <code>Project</code>.
   */   
  def countClasses() : Int = {
    classes.size
  }
  
  
  /** 
   * The Method Hiding Factor of the <code>Project</code> as defined in the paper. 
   * 
   * @see metricsplugin.handlers.MetricsCalculator#getAHF.
   * @return The MHF.
   */ 
  def getMHF() : Double = {
    var sum = 0.0
    for((className,meths) <- methods) {
      var totalPublic = 0.0
      for(m <- meths ; if m._2 != "private") {
        totalPublic += 1
      }
      totalPublic = totalPublic / (countClasses - 1)
      meths.toList.foreach(x => sum += (1.0-totalPublic))
    }
    Console println "\ngetMHF:\n\tx: " + sum    
    Console println "getMHF:\n\ttotal: " + sum / getTotalMethods * 100 + "%"
    sum.toDouble / getTotalMethods * 100
  }
  
  /** 
   * The Attribute Hiding Factor of the <code>Project</code> as defined in the paper.
   * 
   * @see metricsplugin.handlers.MetricsCalculator#getMHF
   * @return The total number of fields/attributes in the <code>Project</code>.
   */ 
  def getAHF() : Double = {
    var sum = 0.0
    for((className,flds) <- fields) {
      var totalPublic = 0.0
      for(m <- flds ; if m._2 != "private") {
        totalPublic += 1
      }
      totalPublic = totalPublic / (countClasses - 1)
      flds.toList.foreach(x => sum += (1.0-totalPublic))
    }
    Console println "\ngetAHF:\n\tx: " + sum    
    Console println "getAHF:\n\ttotal: " + sum / getTotalFields * 100 + "%"
    sum.toDouble / getTotalFields * 100
  }
  
  /** 
   * Determines if <code>client</code> access a field or method from 
   * <code>supplier</code>.
   * 
   * @param client The client class.
   * @param supplier The supplier of the method/field - The Type that contains the method or field.
   * @return 1, if <code>client</code> access <code>supplier</code>, 0 otherwise.
   */ 
  private def isClient(client:TypeDeclaration, supplier:TypeDeclaration) : Int = {
    /*
     * The naive approach documented else in <code>isClient</code> doesn't
     * work in all the cases.  I.e., comments although aren't semantic links
     * are treated as such.  We need to traverse the AST.
     * 
     * We need to check the following;
     * 
     * 1. imports, if they include the supplier classes or its package.
     * 2. field types and their corresponding assignments (which might contain a reference
     * to the class).
     * 3. methods in their full glory.
     * 4. inner classes as well.
     * 
     * 
     * It would be great if the API directly supported checking if something was in scope, in 
     * this case if the import has been resolved.  And if a class, A, is used in an ICU.
     */
    
    var cTB = client.resolveBinding
    var sTB = supplier.resolveBinding
    // if we're looking at the same type, just return
    if(cTB.isEqualTo(sTB)) {
      return 0
    } else {
      
    }
    
    // get the ITypeBinding of each TypeDeclaration in the supplier
    val sMethodBindings = supplier.resolveBinding.getDeclaredMethods.toList
    val sFieldBindings = supplier.resolveBinding.getDeclaredFields.toList
    
    var found = 0
    client.accept(new ASTVisitor() {
      override def visit(node : MethodInvocation) = {        
        //mths += (node.getName.toString,getModifier(node))
        var methodBinding = node.resolveMethodBinding
        if(sMethodBindings.exists(x => x.isEqualTo(methodBinding))) {
          //Console println "client field: " + client.getName + ", " + methodBinding
          found = 1
        } else {
          
        }
        true
      }
      override def visit(node : FieldAccess) = {
        //mths += (node.getName.toString,getModifier(node))
        var fieldBinding = node.resolveFieldBinding
        if(sFieldBindings.exists(x => x.isEqualTo(fieldBinding))) {
          //Console println "client method: " + client.getName + ", " + fieldBinding
          found = 1
        } else {
          
        }
        true
      }
    })    
    return found
  }
  
  /** 
   * Get the coupling factor of the <code>Project</code>.  See paper for more 
   * information.
   * 
   * @return The coupling factor.
   */ 
  def getCouplingFactor() : Double = {
    /*
     * possibe strategy; look at every TypeDeclaration (classTD) pairwise and 
     * see if there is a link to the first in the second.  This should be 
     * straight forward since we keep a record of all TDs.
     * 
     * isClient returns 0 if c1 and c2 are the same class.
     */
    var x = 0
    for(c1 <- classTD ; c2 <- classTD) {
      x += isClient(c1,c2)
    }
    Console println "\n\ngetCouplingFactor:\n\tx: " + x 
    var c = countClasses 
    Console println "getCouplingFactor:\n\ttotal: " + x.toDouble / (c*c - c) * 100 + "%\n\n"
    x.toDouble / (c*c - c).toDouble * 100    
  }

  /**
   * Convenience aliasing for type information used later on
   */
  private type NewMethods = Int
  /**
   * Convenience aliasing for type information used later on
   */
  private type OverridingMethods = Int
  /**
   * Convenience aliasing for type information used later on
   */  
  private type DescendantsCount = Int  
  
  /** 
   * See the paper for more information, but this is essentially another convience method.
   * 
   * @param primaryType The Class/Type.
   * @return The number of overriding methods, new methods and the descendants count, as
   * a triple.
   */
  private def getPolymorphismInfo(primaryType:IType) : (OverridingMethods,
                                                        NewMethods,
                                                        DescendantsCount) = {
    
    // most things are self explanatory in this method.
    val hierarchy = primaryType.newTypeHierarchy(new org.eclipse.core.runtime.NullProgressMonitor)
    val superClasses = hierarchy.getAllSuperclasses(primaryType).toList
    val descendants = hierarchy.getAllSubtypes(primaryType).toList
    
    var inheritedMeths:List[IMethod] = List.flatten(
      superClasses.map(_.getMethods.filter(
        x => if(!countConstructors) { !x.isConstructor } else { true } ).toList))
    val mths:List[IMethod] = primaryType.getMethods.
      filter(x => if(!countConstructors) { !x.isConstructor } else { true } ).toList

    val tBinding:ITypeBinding = classTD.
      find(x => x.getName.toString == primaryType.getElementName) match {
        case Some(x) => x.resolveBinding
        case None    => null
      }
    
    val methodBindings:List[IMethodBinding] = tBinding.getDeclaredMethods.toList            
        
    val superClassBindings:List[ITypeBinding] = classTD.
      filter(x => superClasses.exists((y:IType) => y.getElementName == x.getName.toString)).
      map(_.resolveBinding).toList + objectTB
    
    val superMethodBindings:List[IMethodBinding] = 
      List.flatten(superClassBindings.map(_.getDeclaredMethods.toList))

    val supMB = scala.collection.mutable.Set[IMethodBinding]()
    superMethodBindings.foreach(x =>
      if(!supMB.exists(y => x.toString == y.toString)) { supMB += x} else { } )
    //Console println "supMB: " + supMB.size
    //supMB.foreach(Console println _)
    //Console println
        
    var overridingMethods = 0
    var newMethods = 0
    for(m1 <- supMB) {
      var methodFound = methodBindings.
      find(s => s.overrides(m1) ||  m1.toString == s.toString) match {
        case Some(f) => f
        case None    => null
      }

      if(methodBindings.exists(s => s.overrides(m1) ||  m1.toString == s.toString)) {
        //Console println m1 + " overrides + " + methodFound;
        overridingMethods += 1
        
      } else  {
        //Console println m1 + " doesn't overrides + " + m3
        //newMethods += 1        
      }
    }
     
    val methodsInClass = methodBindings.length
    return (overridingMethods,methodsInClass-overridingMethods,descendants.length)
  }  
  
  /** 
   * The Polymorphism Factor, as defined in the paper.
   * 
   * <p>
   * (The sum of the number of overriding methods in each class i) / 
   * (The sum of new methods in each class i x descendants of each class i)
   * </p>
   * 
   * @return The Polymorphism Factor.
   */ 
  def getPolymorphismFactor() : Double = {
    // straight forward encoding from the paper
    var sumOverridingMethods = 0.0
    var newMethodsXDescendants = 0.0  
    for(c <- classTypes) yield {
      val info:(OverridingMethods,NewMethods,DescendantsCount) = getPolymorphismInfo(c);
      /* Console println "\nClass: " + c.getElementName + 
        "\n\tOverridingMethods: " + info._1 +
        "\n\tNewMethods: " + info._2 +
        "\n\tDescendantsCount: " + info._3 */
      sumOverridingMethods += info._1
      newMethodsXDescendants += info._2 * info._3
    }
    Console println "\nPF: " + sumOverridingMethods / newMethodsXDescendants * 100 + "%"
    sumOverridingMethods / newMethodsXDescendants * 100
  }
  
  /* 
   * Note: a lot of the methods below cane be refactored because the control structure is
   * roughly the same. 
   */
  
  /** 
   * This is the helper method for the MIF and AIF metrics, i.e., <code>M_a(C_i)</code>.
   * 
   * @param isMeth True, if we wish to calculate the possible invokations for methods, 
   * false otherwise.
   * @param className The class name.  Also defined as <code>i<code> in the paper.
   * @return The total number of fields/attributes in the <code>Project</code>.
   */ 
  private def getPossibleInvokations(isMeth:Boolean,className:ClassName) : Int = {
    /*
     * I some what doubt that only "private" is not visible to the outside world.  
     * For example, a Class A, not the same package as Class B cannot access the 
     * anything declared with a "protected" modifier.  Unfortunately due to time 
     * constraints I wasn't able to take this into account.
     */
    
    var x = if(isMeth) { methods } else { fields }
        
    var visibile : List[(String, Visibility)] = x.get(className) match {
      case Some(l) => l.filter(_._2 != "private")
      case None    => List[(String, Visibility)]()
    }
    
    var countOfMethOrAttr = if(isMeth) { 
      getMethodCountInClass(className) 
    } else { 
      getAttributeCountInClass(className)
    }
    
    val iType = classTypes.find(x => x.getElementName == className) match  { 
      case Some(it) => it
      case None     => null
    }
    
    var r = countOfMethOrAttr + getInherited(isMeth,iType)
    if(r==0) { return 1 } else { return r }
  }
  
  /** 
   * The number of methods/attributes inherited in a <code>IType</code>, also an 
   * <code>TypeDeclaration</code>
   * 
   * @param isMeth True, if we wish to calculate the possible invokations for methods, 
   * false otherwise.
   * @param primaryType The <i>Type</i>.  You can obtain this from an <code>TypeDeclaration</code>.
   * @return The number of methods/attributes inherited.
   */ 
  private def getInherited(isMeth:Boolean, primaryType:IType) : Int = {
    
    // the type hierarchy allows us to get all the methods defined the superclasses.
    val hierarchy:ITypeHierarchy = 
      primaryType.newTypeHierarchy(new org.eclipse.core.runtime.NullProgressMonitor)
    // store all the superclasses
    val superClasses:List[IType] = hierarchy.getAllSuperclasses(primaryType).toList
    
    /* 
     * The inherited methods, i.e., all the methods from the superclasses.  This is a
     * concatenation of the methods in each of the classes
     */
    var inheritedMeths:List[IMethod] = List.flatten(
      superClasses.map(_.getMethods.filter(
        x => if(!countConstructors) { !x.isConstructor } else { true } ).toList))
    var inheritedFlds:List[IField] = 
      List.flatten(superClasses.map(_.getFields.toList))
    
    /*
     * stores all the methods and fields of the current class, represented by 
     * <code>primaryType</code>.  Note: We're ignoring constructors again, 
     * if the countConstructors == false.
     */
    val mths:List[IMethod] = primaryType.getMethods.
      filter(x => if(!countConstructors) { !x.isConstructor } else { true } ).toList
    val flds:List[IField] = primaryType.getFields.toList

    /*
     * We need the a ITypeBinding to determine if there is an 
     * overriding of this method, and it seems that it's only 
     * available from a <code>TypeDeclaration</code>.  So the below 
     * finds the <code>ITypeBinding</code> for the current <code>primaryType</code>.  
     */ 
    val tBinding:ITypeBinding = classTD.
      find(x => x.getName.toString == primaryType.getElementName) match {
        case Some(x) => x.resolveBinding
        case None    => null
      }
    
    /*
     * The Bindings; <code>IMethodBinding</code> and <code>IVariableBinding</code> and
     * allow use to resolve overloading instead of using defining the semantics by reading the
     * Java Language Specification.  Not an easy task.  Thankfully, the <code>ITypeBinding</code> 
     * does this.
     */
    val methodBindings:List[IMethodBinding] = tBinding.getDeclaredMethods.toList
    val fieldBindings:List[IVariableBinding] = tBinding.getDeclaredFields.toList
            
    /*
     * Get the <code>ITypeBinding</code> for each super class
     */    
    val superClassBindings:List[ITypeBinding] = classTD.
      filter(x => superClasses.exists((y:IType) => y.getElementName == x.getName.toString)).
    map(_.resolveBinding).toList// + objectTB
    
    // if true we work it out for a method and not a field
    if(isMeth) {
      val superMethodBindings:List[IMethodBinding] = 
        List.flatten(superClassBindings.map(_.getDeclaredMethods.toList))
      val supMB = scala.collection.mutable.Set[IMethodBinding]()
      superMethodBindings.foreach(x => 
        if(!supMB.exists(y => x.toString == y.toString)) { supMB += x} else { } )
      
      // sum of the number of overriding methods
      var sum = 0
      for(m1 <- supMB) {
        var methodFound = methodBindings.
          find(s => s.overrides(m1) ||  m1.toString == s.toString) match {
            case Some(f) => f
            case None    => null
          }    
        /*
         * the extra condition is necessary because java.lang.Object has unresolved type bindings.
         * [unresolved] public boolean equals(java.lang.Object)
         * ... 
         * 
         * probably a bug in Eclipse
         */
        if(methodBindings.exists(s => s.overrides(m1) ||  m1.toString == s.toString)) {
        } else {
          // the method is inherited, so increment sum
          sum += 1
        }
      }      
      return sum
    } else {      
      var sum = 0
      for(f1 <- inheritedFlds) {
        var fieldFound = flds.find(_.getElementName == f1.getElementName) match { 
          case Some(f) => f
          case None    => null
        }        
        if(flds.exists(x => x.getElementName == f1.getElementName)) {
          // 
        } 
        else { 
          // the field is inherited, so increment sum
          sum += 1
        }
      }
      return sum
    }
  }

  /** 
   * The MIF, as defined in the paper.
   * 
   * @see metricsplugin.handlers.MetricsCalculator#getAIF.
   * @return The MIF.
   */  
  def getMIF() : Double = {
    // note i don't comment on this much and it's straight forward
    // copy of the formula from the paper
    
    /* 
     * Stores the total number of methods inherited, i.e.,
     * <code>M_i(C_i)</code>
     */
    var inheritedMethods = 0
    for(c <- classTypes) {
      inheritedMethods += getInherited(true, c)
    }

    /* 
     * Stores the total number of invokable methods, i.e.,
     * <code>M_a(C_i)</code>
     */
    var invokableMethods = 0
    for(c <- methods.keys) {
      // true is passed, as we wish to calculate invokations for methods not fields
      invokableMethods += getPossibleInvokations(true,c)
    }
    
    Console println "\ninherited: " + inheritedMethods
    Console println "invokable: " + invokableMethods    
    Console println "\nMIF: " + inheritedMethods.toDouble / invokableMethods * 100.0 + "%"
    
    return inheritedMethods.toDouble / invokableMethods * 100
  }

  /** 
   * The AIF, as defined in the paper. 
   * 
   * @see metricsplugin.handlers.MetricsCalculator#getMIF
   * @return The AIF.
   */ 
  def getAIF() : Double  = {
    var inheritedAttributes = 0
    for(c <- classTypes) {
      inheritedAttributes += getInherited(false, c)
    }
    // note methods.keys return all the class names
    var invokableAttributes = 0
    for(c <- methods.keys) {
      invokableAttributes += getPossibleInvokations(false,c)
    }
    Console println "\ninherited: " + inheritedAttributes
    Console println "invokable: " + invokableAttributes
    Console println "AIF: " + 
    inheritedAttributes.toDouble / invokableAttributes * 100 + "%"
    inheritedAttributes.toDouble / invokableAttributes * 100
  }
  
  /**
   * The total number of methods in in the <code>Project</code>.
   * 
   * @see metricsplugin.handlers.MetricsCalculator#countConstructors.
   * @param className The class in question.
   * @return The total number of fields/attributes in the <code>Project</code>, or 0 
   * is the class doesn't exist.
   */
  private def getMethodCountInClass(className:ClassName) : Int = {
    methods.get(className) match {
      case Some(l) => 
        //Console println "\tgetMethodCountInClass: "+l.length; 
        l.length
      case None    => 0
    }    
  }

  /** 
   * The total number of fields/attributes in the <code>Project</code>.
   * 
   * @param className  
   * @return The total number of fields/attributes in the <code>Project</code>, or 0 
   * is the class doesn't exist.
   */  
  private def getAttributeCountInClass(className:ClassName) : Int  = {
    fields.get(className) match {
      case Some(l) => 
        //Console println "\tgetAttributeCountInClass: "+l.length; 
        l.length
      case None    => 0
    }     
  }    

  /** 
   * Help method to print the all the fields in all the classes.
   */ 
  private def printFields() : Unit = {
    if(fields==null) ()
      
    Console println "Fields: "; // + fields
    for((c,ms) <- fields) {
      Console println "Fields In class: " + c
      getAttributeCountInClass(c)
      for(x <- ms) {
        Console println "\t" + x
      }
    }
  }
  
  /** 
   * Help method to print the all the methods in all the classes.
   */  
  private def printMethods() : Unit = {
    if(methods==null) ()
      
    Console println "Methods: "; // + fields
    for((c,ms) <- methods) {
      Console println "Methods In class: " + c
      getMethodCountInClass(c)
      for(x <- ms) {
        Console println "\t" + x
      }
    } 
  }


  /** 
   * Traverses the entire <code>ICompilationUnit</code> and adds everything into 
   * the <code>fields</code> and <code>methods</code>.
   * 
   * @see metricsplugin.handlers.MetricsCalculator#fields
   * @see metricsplugin.handlers.MetricsCalculator#methods
   * @param icu The source file
   * 
   */ 
  private def setMethodsAndFields(icu:ICompilationUnit) : Unit = {
    /**
     * Get modifier <code>node</code> is declared with.
     * 
     * @param node The node.
     * @return Either private, public, protected or empty depending on the 
     * modifier of <code>node</code>.
     */
    def getModifier(node : BodyDeclaration) : String = {   
      /* for reasons unknown to the author Modifier#isPrivate and Modifier#isPublic
       * both return false regardless if modifier is in fact private or public, I
       * guess this is a Java byte code issue.
       */
      var modifiers = node.modifiers
      var newModifiers = List[String]()
      for(m <- modifiers.toArray) {
        if(m.isInstanceOf[Modifier]) {
          var mod = m.asInstanceOf[Modifier]
          newModifiers += mod.toString
        } else {
          
        }
      }
      
      newModifiers match {      
        case List("private")   => "private"
        case List("public")    => "public"
        case List("protected") => "protected"
        case "private"   :: xs => "private"
        case "public"    :: xs => "public"
        case "protected" :: xs => "protected"
        case "final"     :: xs => "final"
        case "abstract"  :: xs => "abstract"
        case List()            => "empty"
      }
    }
    
    /*
     * the one below fails, we can declare classes within interfaces, sp
     * disregarding an ICompilationUnit simply because it's an interface will 
     * miss these cases
     */
    // we aren't considering interfaces so just return when we encounter one
    // if(icu.findPrimaryType.isInterface) { return () }
    
    //var icu:ICompilationUnit = p.asInstanceOf[ICompilationUnit];
    var parser:ASTParser = ASTParser.newParser(AST.JLS3);
    parser.setSource(icu);
    parser.setResolveBindings(true);
    var root:CompilationUnit = (parser.createAST(null)).asInstanceOf[CompilationUnit];
    
    // get the TypeBinding for Object
    objectTB = root.getAST.resolveWellKnownType("java.lang.Object")
    //Console println "Objected type binding: " + objectTB
    //System.in.read
    //System.exit(1)
    
    /* 
     * DONE 1. might  consider nested classes and whether
     * an interface should be counted as a class
     *
     * 2. I assume when adding methods and fields that if their corresponding 
     * parent class isn't in the (Hash)Map I simply ignore, because they must 
     * be interfaces since I don't add 
     * 
     */
    
    root.accept(new ASTVisitor() {
        private def getDefiningClassName(node:BodyDeclaration) = {
          node.getParent.asInstanceOf[TypeDeclaration].getName.toString
        }
      
        /*
         private def getFieldOrMethodInfo(node:BodyDeclaration) = {
         (node.getName.toString,getModifier(node))
         }
         */
      
        override def visit(node : TypeDeclaration) = {
          // ignore interfaces
          if(!node.isInterface) {
            // add the inner class or topmost class to the array of class types
            classTD += node
            methods += (node.getName.toString -> List[(MethodName,Visibility)]())
            fields += (node.getName.toString -> List[(FieldName,Visibility)]())
          }
          true
        }
      
        override def visit(node : MethodDeclaration) : Boolean = {
          // if it's a constructor and we're ignoring them, just return
          if(node.isConstructor && !countConstructors) { return true } else { }
        
          val definingClass = getDefiningClassName(node)
          val methodInfo = (node.getName.toString,getModifier(node))
          methods.get(definingClass) match {
            case Some(ms) => methods += (definingClass -> (ms ++ List(methodInfo)))
            case None    => () //methods += (definingClassName -< methodInfo)
          }
          true
        }
      
        override def visit(node : FieldDeclaration) = {
          val definingClass = getDefiningClassName(node)
          import scala.collection.jcl.Conversions.convertList
          var flds = List[(FieldName,Visibility)]()
          
          // for each field found add it's name and visibility
          for(v <- convertList(node.fragments)) {
            flds += (v.asInstanceOf[VariableDeclarationFragment].getName.toString,getModifier(node))
          }
          
          fields.get(definingClass) match {
            case Some(fs) => fields += (definingClass -> (fs ++ flds))
            case None     => () //methods += (definingClassName -< methodInfo)
          }       
        
          true
        }
      }
    )
    
    // ensure that the Visibility isn't null or the empty string ("")
    List.flatten(methods.values.toList).map(m =>
      assert(m._1 != "" && m._1 != null && m._2 != "" && m._2 != null, 
             "we have a (MethodName,Visibility) that is potentially null: " + m))
    
    List.flatten(fields.values.toList).map(f =>
      assert(f._1 != "" && f._1 != null && f._2 != "" && f._2 != null, 
             "we have a (FieldName,Visibility) that is potentially null: " + f)
    )
  }

  /** 
   * Auxilary method to setup the data structures the class uses.
   */   
  private def setup() : Unit = {
    var root:IWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
    var workspace:IWorkspace = ResourcesPlugin.getWorkspace();
    var javaModel:IJavaModel = JavaCore.create(workspace.getRoot());
    var cwProject = javaModel.getJavaProject(proj);
    
    /* 
     * get all the packages in the project, note these include the JRE and other
     * such libraries
     */
    var roots:Array[IPackageFragmentRoot] = cwProject.getAllPackageFragmentRoots();
    for(r <- roots) {
      for(p <- r.getChildren) {
        // p is a class in the default package
        if(p.getElementType() == IJavaElement.COMPILATION_UNIT) {
          val x = p.asInstanceOf[ICompilationUnit]
          // add the methods and fields of p
          setMethodsAndFields(x)
          classes += x
        } else {
          /* it's a package so loop to get all the classes, which are
           * the children */
          var frag : IPackageFragment = 
            (p.getAdapter(classOf[IPackageFragment])).asInstanceOf[IPackageFragment];
          // for all the source files
          for(p1 <- frag.getCompilationUnits) {
            // if indeed it is a source file
            if(p1.getElementType() == IJavaElement.COMPILATION_UNIT) {
              // add the methods and fields of p1
              setMethodsAndFields(p1)
              classes += p1.asInstanceOf[ICompilationUnit]
            }
          }
        }
      }
    }
    
    // store everything single type declared in all <code>ICompilationUnit</code>s.
    classTypes = List.flatten(classes.map(_.getAllTypes.toList.filter(_.isClass)))
  }   
}
