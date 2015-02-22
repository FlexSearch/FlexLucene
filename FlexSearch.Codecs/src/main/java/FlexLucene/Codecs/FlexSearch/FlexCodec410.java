// ----------------------------------------------------------------------------
// (c) Seemant Rajvanshi, 2013
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------
package FlexLucene.Codecs.FlexSearch;

import org.apache.lucene.codecs.lucene410.Lucene410Codec;

public final class FlexCodec410 extends FlexCodecBase {

    public FlexCodec410() {
        super("FlexCodec410", new Lucene410Codec());
    }
}
