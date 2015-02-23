/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package FlexLucene.Codecs.FlexSearch;

import org.apache.lucene.codecs.Codec;
import org.apache.lucene.codecs.lucene50.Lucene50Codec;

/**
 *
 * @author Seemant
 */
public final class FlexCodec50 extends FlexCodecBase {

    public FlexCodec50() {
        super("FlexCodec50", new Lucene50Codec());
    }
    
}
