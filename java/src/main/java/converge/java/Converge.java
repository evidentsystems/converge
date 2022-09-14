package converge.java;

import converge.domain.ConvergentRef;
import clojure.lang.IFn;
import clojure.java.api.Clojure;

import java.io.File;

public class Converge {
    // private static final IFn reset = Clojure.var("clojure.core", "reset!");
    // private static final IFn deref = Clojure.var("clojure.core", "deref");
    private static final IFn syncDirectoryClojure = Clojure.var("converge.storage.filesystem", "sync-directory");

    public static String syncDirectory(ConvergentRef ref, File directory) {
        if (directory.isDirectory()) {
            return (String) syncDirectoryClojure.invoke(ref, directory.getName());
        } else {
            throw new IllegalArgumentException("Syncing a convergent ref requires a directory");
        }
    }
}
