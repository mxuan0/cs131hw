import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) {
        int n = v.length;
        value = new AtomicIntegerArray(n);
        for (int i=0; i<n; i++)
            value.set(i,(int)v[i]);
        maxval = 127;
    }

    GetNSetState(byte[] v, byte m) {
        int n =v.length;
        value =new AtomicIntegerArray(n);
        for (int i=0; i<n; i++)
            value.set(i,(int)v[i]);
        maxval = m;
    }

    public int size() { return value.length(); }

    public byte[] current() {
        int n =value.length();
        byte[] v =new byte[n];
        for (int i=0; i<n; i++)
            v[i]= (byte)value.get(i);
        return v;
    }

    public boolean swap(int i, int j) {
        int v1=value.get(i);
        int v2=value.get(j);
        if (v1 <= 0 || v2 >= maxval) {
            return false;
        }
        value.set(i,v1-1);
        value.set(j,v2+1);
        return true;
    }
}
