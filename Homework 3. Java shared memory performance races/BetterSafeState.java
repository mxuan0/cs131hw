import java.util.concurrent.locks.ReentrantLock;

class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;
    //    private static final int MAX_AVAILABLE = 1;                                                                             
    // private final Semaphore available = new Semaphore(MAX_AVAILABLE, true);                                                    
    private final ReentrantLock lock = new ReentrantLock();

    BetterSafeState(byte[] v) { value = v; maxval = 127; }

    BetterSafeState(byte[] v, byte m) { value = v; maxval = m; }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
        if (!(value[i] <= 0 || value[j] >= maxval)) {
        //      try{                                                                                                              
        //      available.acquire();                                                                                              
        //      }catch (InterruptedException exc) {                                                                               
        //  System.out.println(exc);                                                                                              
        //}                                                                                                                       
            lock.lock();  // block until condition holds                                                                         \
                                                                                                                                  

            if (value[i] <= 0 || value[j] >= maxval) {
                //available.release();                                                                                            
                lock.unlock();                                                                                                   \

                return false;
            }
            value[i]--;
            value[j]++;
            //available.release();                                                                                                
            lock.unlock();                                                                                                       \

            return true;
        }
        return false;
    }
}
