// ----------------------------------------------------------------------------
// (c) FlexSearch, 2011 - 2015
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------
package FlexLucene.Codecs.FlexSearch;

import org.apache.lucene.codecs.*;

public abstract class FlexCodecBase extends FilterCodec {

    private static final PostingsFormat postingsFormat = new FlexPerFieldPostingsFormat();

    public FlexCodecBase(String codecName, Codec wrappingCodec) {
        super(codecName, wrappingCodec);
    }

    @Override
    public final PostingsFormat postingsFormat() {
        return postingsFormat;
    }
}
