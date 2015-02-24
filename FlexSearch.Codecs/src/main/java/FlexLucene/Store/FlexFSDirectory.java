/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package FlexLucene.Store;

import org.apache.lucene.store.FSDirectory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import org.apache.lucene.store.LockFactory;

import org.apache.lucene.util.IOUtils;

/**
 *
 * @author vnegacevschi
 */
public abstract class FlexFSDirectory extends FSDirectory {

    protected FlexFSDirectory(Path path, LockFactory lockFactory) throws IOException {
        super(path, lockFactory);
    }

    @Override
    public void renameFile(String source, String dest) throws IOException {
        ensureOpen();
        Files.move(directory.resolve(source), directory.resolve(dest), StandardCopyOption.REPLACE_EXISTING);
        // TODO: should we move directory fsync to a separate 'syncMetadata' method?
        // for example, to improve listCommits(), IndexFileDeleter could also call that after deleting segments_Ns
        IOUtils.fsync(directory, true);
    }
}
