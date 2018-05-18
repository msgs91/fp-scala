package ds;

import java.util.LinkedList;

public class RabinKarp {

    public static LinkedList<Integer> isSubString(String pattern, String text, int d){
        int m = pattern.length();
        int n = text.length();
        Integer patternHash = getHash(pattern, d);
        Integer subStringHash = getHash(text.substring(0, m), d);
        int dPowm = (int) Math.pow(d, m-1);
        LinkedList<Integer> matchingIndices = new LinkedList<>();

        for(int s = 0; s <= n-m; s++){
//            System.out.println("patternHash " + patternHash + " subStringHash " + subStringHash);
            if(patternHash.equals(subStringHash)){
                if(pattern.equals(text.substring(s, s+m))){
                    matchingIndices.addLast(s);
                }
            }
            if(s < n-m){
                subStringHash = d * (subStringHash % dPowm) + Integer.parseInt(text.substring(s+m, s+m+1));
            }
        }
        return matchingIndices;
    }

    public static int getHash(String s, int base){
        int hash = 0;

        for(int i=0; i < s.length(); i++){
            hash = Integer.parseInt(s.substring(i, i+1)) + hash * base;
        }
        return hash;
    }

    public static void main(String[] args){
        String text = "11121121211112";
        String pattern = "1111";
        LinkedList<Integer> result = isSubString(pattern, text, 10);
        for(int pos: result){
            System.out.print(pos);
        }
        System.out.println();
    }
}
