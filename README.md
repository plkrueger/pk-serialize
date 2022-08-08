# pk-serialize
 Common Lisp Serialization

This provides a serialization capability for storing and restoring a collection of lisp objects. It is not meant to replace databases for very large amounts of data. All data is stored in a way that is readable by normal lisp read functions. It does have some advantages over databases in that the structures that are  created when data is restored from disk may be arbitrarily connected, even in circular reference loops.  Non-list objects (e.g. class or structure instances, etc) that are eq prior to the dump will be eq in the restored objects. Lists that are equal prior to the dump will be equal after the restore, but in general will not be eq, even if they were so prior to the dump. Arbitrary lisp objects may be dumped. Object and structure instances, hash-tables, arbitrary arrays, functions, etc. should all be restored correctly.

The user must specify an initial list of objects to be dumped. One useful idiom is to use the instance-tracking class as a :metaclass for any class you define for which you want all instances to be dumped. As instances are created they will be added to class-allocated slot. When dumping, it is only necessary to put the class into the list of objects that need to be dumped and then all instances will also be dumped. Note that if you then restored from the saved file without deleting the existing instances of that class, you would duplicate all of the instances that had been dumped. And if you want to dump all instances of all classes for which you have specified instance-tracking (https://github.com/plkrueger/instance-tracking) as the :metaclass, then you can simply call (pk-inst-track::instance-tracking-classes) to get a list of all those classes. If that is passed to dump-to-file as the dumpfile argument, then all instances of all those classes will be dumped.

If you want to restrict the slots that are dumped for any class instance you can define the method slots-to-dump:
    (defmethod slots-to-dump ((obj <your-class>)) ...
that returns a list of slot-names that should be dumped. 

Alternatively, if you want all slots with a few exceptions you can define the method slots-to-not-dump:
    (defmethod slots-to-not-dump ((obj <your-class>)) ...
that returns a list of slot-names that should not be dumped (all others WILL be dumped).

Note that when instances are restored, the initialize-instance methods for that class will NOT be run.  So if anything additional needs to be done to set values for instance slots that are not dumped and restored, that should be done after the restore is completed.

You can also put individual symbol names into the list and the symbol-value of that symbol will be saved and restored when the data is re-loaded.

Test functions at the end of the source code can be used as an example of how to dump and restore lisp data.